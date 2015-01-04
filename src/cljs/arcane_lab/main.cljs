(ns arcane-lab.main
  (:require [cljs.core.async :as async :refer [>! <!]]
            [cljs-uuid-utils :refer [make-random-uuid]]
            [jamesmacaulay.zelkova.signal :as sig]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.time :as time]
            [jamesmacaulay.async-tools.core :as tools]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def rand-uuid make-random-uuid)

(enable-console-print!)

;;
;; Constants
;;

(def card-width 222)
(def card-height 319)

(def half-card-width (int (/ card-width 2)))
(def half-card-height (int (/ card-height 2)))

(def gutter (int (/ card-width 8)))
(def half-gutter (int (/ gutter 2)))
(def pile-stride (int (/ card-height 9.25)))
(def pile-spacing (+ card-width gutter))

;;
;; Card-Generating Functions
;;

(defn forest []
  {:name "Forest"
   :id (rand-uuid)
   :img-src "forest.jpg"
   :x half-gutter
   :y half-gutter
   :selected? false})

(defn card-at
  ([card [x y]] (card-at card x y))
  ([card x y] (assoc card :x x :y y)))

;;
;; Selection / Geometric Filtering
;;

(defn within?
  "Returns true if the point at (x,y) is within the box defined by left, right,
  top, and bottom."
  [left right top bottom x y]
  (and (not (> x right))  ; the point is not to the right of the box
       (not (< x left))   ; the point is not to the left of the box
       (not (> y bottom)) ; the point is not below the box
       (not (< y top))))  ; the point is not above the box

(defn selection-edges
  "Given a selection, return the left & right x values and the top & bottom
  y values, in a vector in that order ([l r t b])."
  [selection]
  (let [{[x1 y1] :start, [x2 y2] :stop} selection]
    (vec (concat (sort [x1 x2])
                 (sort [y1 y2])))))

;;
;; Piles
;;

(defn pile-height
  [{:keys [cards] :as pile}]
  (if-not pile
    0
    ;; by induction, each card besides the first is covered by the previous
    (let [covered (dec (count cards))]
      (+ card-height (* pile-stride covered)))))

(defn make-pile
  ([cards] (make-pile cards
                      (-> (map :x cards) sort first)
                      (-> (map :y cards) sort first)))
  ([cards x y]
   (let [reposition (fn [i card] (assoc card :x x, :y (+ y (* i pile-stride))))
         repositioned (map-indexed reposition cards)]
     {:cards repositioned, :x x, :y y})))

(defn add-pile
  [state pile]
  (let [{:keys [x y]} pile]
    (-> state
       (update-in [:piles y] (fnil identity (sorted-map)))
       (assoc-in [:piles y x] pile))))

(defn remove-pile
  [state x y]
  (let [row (get-in state [:piles y])]
    (if (and (contains? row x) (= (count row) 1))
      (update-in state [:piles] dissoc y)
      (update-in state [:piles y] dissoc x))))

(defn pile-selected?
  [selection pile]
  (and (boolean selection)
       (let [[l r t b] (selection-edges selection)
             {x :x, y :y} pile
             height (pile-height pile)]
         (within? (- l card-width) r (- t height) b x y))))

(defn row-spacings
  "Given the row y locations in an ascending sequence, return a sequence of the
  spacing between each row. Note that the returned sequence will have one less
  element than the input sequence."
  [row-ys]
  (if (= count row-ys 1)
    ()
    (->> (reductions #(- %2 %1) row-ys)
      (drop 1))))

(defn pile-after-selection
  "Given a selection and a pile returns a new pile with the cards that the
  selection hits marked by setting their :selected? field to true."
  [selection pile]
  (cond
    (not selection)
    pile

    (not (pile-selected? selection pile))
    (update-in pile [:cards] (partial mapv #(assoc % :selected? false)))

    :otherwise
    (let [[l r t b] (selection-edges selection)
          {cards :cards} pile
          uncovered (last cards)
          select-uncovd? (let [{y :y} uncovered]
                           (and (not (> y b))
                                (not (< (+ y card-height) t))))
          covered (butlast cards)
          covd-selected? (fn [{y :y}]
                           (and (not (> y b))
                                (not (< (+ y pile-stride) t))))
          selected-covd-ids (->> (filter covd-selected? covered) (map :id))
          selected-ids (if select-uncovd?
                         (concat selected-covd-ids (list (:id uncovered)))
                         selected-covd-ids)
          cards' (loop [cs cards, slctd-ids selected-ids, cs' []]
                   (if-let [{:keys [id] :as c} (first cs)]
                     (let [selected? (= id (first slctd-ids))
                           c' (assoc c :selected? selected?)]
                       (if selected?
                         (recur (next cs) (next slctd-ids) (conj cs' c'))
                         (recur (next cs) slctd-ids (conj cs' c'))))
                     ;; else (no more cards)
                     cs'))]
      (assoc pile :cards cards'))))

(defn move-row
  [piles y y']
  (let [row (get piles y)
        row' (reduce (fn [row {:keys [cards x]}]
                       (assoc row x (make-pile cards x y')))
                     row
                     (vals row))]
    (-> piles
      (dissoc y)
      (assoc y' row'))))

(defn rejigger-rows
  "After placing, the rows may be too far apart if cards were moved from the
  strictly tallest pile in a row, or they may be too close together if new cards
  were moved to the tallest pile in a row. Given the piles map, Return a new
  piles map with the rows exactly as far apart as necessary (i.e., the max pile
  height in each row plus the gutter)."
  [piles]
  (let [ys (keys piles)
        spacings (row-spacings ys)
        spacings' (for [row (butlast (vals piles))]
                    (+ (apply max (map pile-height (vals row))) gutter))
        ys' (reductions + half-gutter spacings')]
    (if (= spacings spacings')
      piles
      (reduce (fn [piles [y y']] (move-row piles y y'))
              piles
              (map vector ys ys')))))

;;
;; Dragging
;;

(def drag-x-offset (/ card-width 2))
(def drag-y-offset (-> (* 0.4 card-height) int))

(defn drag-pile-pos
  "Given the mouse's x & y coordinates, return the position of the drag pile
  such that the mouse is in the center."
  [x y]
  [(- x drag-x-offset)
   (- y drag-y-offset)])

(defn mouse-pos
  [drag-x drag-y]
  [(+ drag-x drag-x-offset)
   (+ drag-y drag-y-offset)])

(defn distance-squared-to
  ([p] (let [{x :x y :y} p] (distance-squared-to x y)))
  ([x y]
   (fn [[cx cy]]
     (let [dx (- x (+ cx half-card-width))
           dy (- y (+ cy half-card-height))]
       (+ (* dx dx) (* dy dy))))))

(defn drag-target
  [drag piles]
  ;; want to find the nearest drop position for the drag, which means:
  ;;   nearest pile or place where new pile could be inserted
  (if drag
    (let [[x y] (mouse-pos (:x drag) (:y drag))
          col-index (quot (- x half-gutter) pile-spacing)
          left-col (-> (* col-index pile-spacing) (+ half-gutter))
          right-col (-> (* (inc col-index) pile-spacing) (+ half-gutter))
          rows (vals piles)
          all-piles (mapcat vals rows)
          row-ys (keys piles)
          row-count (count piles)
          last-row-height (let [last-row-piles (-> (get piles (last row-ys)) vals)]
                            (+ gutter (apply max (map pile-height last-row-piles))))
          row-spacings (conj (vec (row-spacings row-ys))
                             last-row-height)
          [before-and-on after] (split-with #(<= % y) row-ys)
          first-row-y half-gutter
          row-y (or (last before-and-on) first-row-y)
          row-height (cond
                       (= row-count 1) (first row-spacings)
                       (< y first-row-y) (first row-spacings)
                       (not (empty? after)) (nth row-spacings
                                                 (dec (count before-and-on)))
                       :otherwise last-row-height)
          candidates (for [cx [left-col right-col]
                           cy [row-y (+ row-y row-height)]]
                       [cx cy])]
      (first (sort-by (distance-squared-to x y) candidates)))))

;;
;; State Actions
;;

(defn start-drag-action
  [pos]
  (fn [{:keys [piles] :as state}]
    (let [piles-in-selection (filter #(some :selected? (:cards %))
                                     (mapcat vals (vals piles)))
          [x y] pos
          drag-start? (loop [ps piles-in-selection]
                        (if-let [{l :x, pt :y, cards :cards, :as p} (first ps)]
                          (let [r (+ l card-width)
                                t (-> (filter :selected? cards) first :y)
                                b (+ pt (pile-height p))]
                            (if (within? l r t b x y)
                              true
                              (recur (next ps))))
                          ;; else (no more piles)
                          false))]
      (if-not drag-start?
        (dissoc state :drag)
        (let [[drag-pile-x drag-pile-y] (drag-pile-pos x y)
              selected-cards (filter :selected?
                                     (mapcat :cards piles-in-selection))
              drag-pile (make-pile selected-cards drag-pile-x drag-pile-y)
              state' (reduce (fn [state {px :x, py :y, cards :cards}]
                               (if (empty? cards)
                                 (remove-pile state px py)
                                 (add-pile state (make-pile cards px py))))
                             state
                             (map #(update-in % [:cards] (partial remove :selected?))
                                  piles-in-selection))]
          (assoc state' :drag drag-pile))))))

(defn apply-selection
  [state selection]
  (let [{piles :piles} state]
    (if-not selection
      state
      (reduce (fn [state {x :x, y :y, :as pile'}]
              (assoc-in state [:piles y x] pile'))
            state
            (map (partial pile-after-selection selection)
                 (mapcat vals (vals piles)))))))

(defn start-selection-if-not-dragging-action
  [pos]
  (fn [state]
    (if (:drag state)
      state
      (let [selection {:start pos, :stop pos}]
        (-> state
          (apply-selection selection)
          (assoc :selection selection))))))

(defn update-selection-or-drag-destination-action
  [pos]
  (fn [{:keys [selection drag] :as state}]
    (cond
      selection (update-in state [:selection] assoc :stop pos)
      drag (let [[x y] pos
                 [px py] (drag-pile-pos x y)]
             (assoc state :drag (make-pile (:cards drag) px py)))
      :otherwise state)))

(defn stop-selection-or-drag-action
  [_]
  (fn [{:keys [selection drag piles] :as state}]
    (cond
      selection (-> state
                  (apply-selection selection)
                  (dissoc :selection))
      drag (let [[tx ty] (drag-target drag piles)
                 new-pile (if-let [{old-cards :cards} (get-in piles [ty tx])]
                            (make-pile (concat old-cards (:cards drag))
                                       tx, ty)
                            (make-pile (:cards drag) tx ty))]
             (-> state
               (add-pile new-pile)
               (update-in [:piles] rejigger-rows)
               (dissoc :drag)))
      :otherwise state)))

;;
;; Signal Graph
;;

(def initial-state
  (let [blank {:piles (sorted-map)}]
    (add-pile blank (make-pile (repeatedly 7 forest)))))

(def state-signal
  (let [drag-coords (sig/keep-when mouse/down? [0 0] mouse/position)
        dragging? (let [true-on-dragmove (sig/sample-on drag-coords (sig/constant true))]
                    (->> (sig/merge (sig/keep-if not false mouse/down?) true-on-dragmove)
                      sig/drop-repeats))
        start-drag (sig/keep-if identity true dragging?)
        stop-drag (sig/keep-if not false dragging?)
        drag-start-coords (sig/sample-on start-drag mouse/position)
        click-down (sig/keep-if identity true (sig/drop-repeats mouse/down?))
        click-down-coords (sig/sample-on click-down mouse/position)
        click-up (sig/keep-if not false (sig/drop-repeats mouse/down?))
        actions (sig/merge
                  (sig/lift start-drag-action click-down-coords)
                  (sig/lift start-selection-if-not-dragging-action drag-start-coords)
                  (sig/lift update-selection-or-drag-destination-action drag-coords)
                  (sig/lift stop-selection-or-drag-action stop-drag)
                  (sig/lift stop-selection-or-drag-action click-up)
                  (sig/constant identity))]
    (sig/reducep (fn [state action] (action state))
                 initial-state
                 actions)))

;;
;; Rendering
;;

(defn render-selection
  [selection]
  (if selection
    (let [[left right top bottom] (selection-edges selection)]
      (dom/div #js {:id "selection"
                    :className "box"
                    :style #js {:position "absolute"
                                :top top
                                :left left
                                :width (- right left)
                                :height (- bottom top)}}
               nil))))

(defn render-card
  [card]
  (let [{:keys [name img-src x y selected?]} card]
    (dom/div #js {:className (str "card" (if selected? " selected"))
                  :style #js {:position "absolute"
                              :left x
                              :top y}}
             (dom/img #js {:src img-src, :title name
                           :width card-width, :height card-height}))))

(defn render-pile
  [pile selection]
  (let [{cards :cards} (pile-after-selection selection pile)]
    (apply dom/div #js {:className "pile"}
           (map render-card cards))))

(defn render-drag
  [drag piles]
  (if drag
    (let [[tx ty] (drag-target drag piles)]
      (dom/div nil
               (dom/div #js {:id "drag-target"
                             :className "ghost"
                             :style #js {:position "absolute"
                                         :left tx
                                         :top ty
                                         :width card-width
                                         :height card-height}})
               (apply dom/div #js {:id "drag" :className "pile"}
                      (map render-card (:cards drag)))))))

(defn render-hud
  [state]
  (let [no-piles (dissoc state :piles)
        dragged-ids (if (contains? no-piles :drag)
                      (update-in no-piles [:drag :cards] (partial map :id))
                      no-piles)]
    (dom/div #js {:id "hud", :style #js {:position "relative"}}
             (dom/pre nil
                      (dom/b nil
                             (.stringify js/JSON
                               (clj->js dragged-ids) nil 2))))))

(defn render-state
  [state]
  (let [{selection :selection} state
        piles (mapcat vals (-> state :piles vals))]
    (dom/div #js {:id "dom-root"}
             (apply dom/div {:id "piles"} (map #(render-pile % selection) piles))
             (render-drag (:drag state) (:piles state))
             (render-selection selection))))

;;
;; Om App
;;

(def !app-state (sig/pipe-to-atom state-signal))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  !app-state
  {:target (. js/document (getElementById "app"))})

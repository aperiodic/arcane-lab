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

(def half-card-width (quot card-width 2))
(def half-card-height (quot card-height 2))

(def gutter (quot card-width 8))
(def half-gutter (quot gutter 2))
(def pile-stride (quot card-height 9.25))
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
;; Piles
;;

(defn pile-height
  [{:keys [cards] :as pile}]
  ;; by induction, each card besides the first is covered by the previous
  (if-not pile
    0
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
       (update-in [:piles x] (fnil identity (sorted-map)))
       (assoc-in [:piles x y] pile))))

(defn remove-pile
  [state x y]
  (let [column (get-in state [:piles x])]
    (if (and (contains? column y) (= (count column) 1))
      (update-in state [:piles] dissoc x)
      (update-in state [:piles x] dissoc y))))

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

(defn pile-selected?
  [selection pile]
  (and (boolean selection)
       (let [[l r t b] (selection-edges selection)
             {x :x, y :y} pile
             height (pile-height pile)]
         (within? (- l card-width) r (- t height) b x y))))

(defn selected-piles
  [selection pile-grid]
  (if-not selection
    []
    (let [[l r t b] (selection-edges selection)
          min-pile-x (- l card-width)
          x-pass-piles (loop [columns (seq pile-grid), piles []]
                         (if-let [[x column] (first columns)]
                           (cond
                             (> x r) piles
                             (< x min-pile-x) (recur (next columns) piles)
                             :otherwise ; x is in range [min-pile-x, r]
                             (recur (next columns) (concat piles (vals column))))
                           ;; else (no more columns)
                           piles))
          selected? (fn [{y :y, :as pile}]
                      (let [height (pile-height pile)]
                        (and (not (> y b))
                             (not (< (+ y height) t)))))]
      (filter selected? x-pass-piles))))

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

(defn drag-target
  [drag piles]
  ;; want to find the nearest drop position for the drag, which means:
  ;;   nearest pile or place where new pile could be inserted
  (if drag
    (let [[x y] (mouse-pos (:x drag) (:y drag))
          col-index (quot (- x half-gutter) pile-spacing)
          left-col (-> (* col-index pile-spacing) (+ half-gutter))
          right-col (-> (* (inc col-index) pile-spacing) (+ half-gutter))
          columns (vals piles)
          all-piles (mapcat vals columns)
          row-ys (-> (map :y all-piles) distinct sort)
          row-count (count row-ys)
          row-heights (if (= row-count 1)
                        [(apply max (map pile-height all-piles))]
                        (->> (reductions #(- %2 %1) row-ys)
                          (drop 1)))
          [before-and-on after] (split-with #(<= % y) row-ys)
          first-row-y half-gutter
          last-row-height (let [last-row-piles (filter #(= (:y %) (last row-ys))
                                                       all-piles)]
                            (apply max (map pile-height last-row-piles)))
          row-height (cond
                       (< y first-row-y) (first row-heights)
                       (not (empty? after)) (nth row-heights
                                                 (dec (count before-and-on)))
                       :otherwise (+ last-row-height gutter))
          row-y (or (last before-and-on) first-row-y)
          candidates (for [cx [left-col right-col]
                           cy [row-y (+ row-y row-height)]]
                       [cx cy])
          distance-squared (fn [[cx cy]]
                             (let [dx (- x (+ cx half-card-width))
                                   dy (- y (+ cy half-card-height))]
                               (+ (* dx dx) (* dy dy))))
          sorted-candidates (sort-by distance-squared candidates)]
      (first sorted-candidates))))

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
              (assoc-in state [:piles x y] pile'))
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
                 new-pile (if-let [{old-cards :cards} (get-in piles [tx ty])]
                            (make-pile (concat old-cards (:cards drag))
                                       tx, ty)
                            (make-pile (:cards drag) tx ty))]
             (-> state
               (add-pile new-pile)
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
        actions (sig/merge
                  (sig/lift start-selection-if-not-dragging-action drag-start-coords)
                  (sig/lift stop-selection-or-drag-action stop-drag)
                  (sig/lift update-selection-or-drag-destination-action drag-coords)
                  (sig/lift start-drag-action click-down-coords)
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

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

(def gutter (quot card-width 8))
(def half-gutter (quot gutter 2))
(def pile-stride (quot card-height 9.25))

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
  (let [covered (dec (count cards))]
    (+ card-height (* pile-stride covered))))

(defn add-pile
  [state cards]
  (let [x (->> (map :x cards) sort first)
        y (->> (map :y cards) sort first)
        cards' (map-indexed (fn [i card] (assoc card :x x, :y (+ y (* i pile-stride))))
                            cards)
        pile {:x x, :y y, :cards cards'}]
    (-> state
      (update-in [:piles x] (fnil identity (sorted-map)))
      (assoc-in [:piles x y] pile))))

;;
;; Selection Filtering
;;

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
         (and
           (not (> x r)) ; pile is not to the right of the selection
           (not (< x (- l card-width))) ; pile is not to the left of the selection
           (not (> y b)) ; pile is not below the selection
           (not (< y (- t height))))))) ; pile is not above the selection

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
                           c' (if selected?
                                (assoc c :selected? true)
                                c)]
                       (if selected?
                         (recur (next cs) (next slctd-ids) (conj cs' c'))
                         (recur (next cs) slctd-ids (conj cs' c'))))
                     ;; else (no more cards)
                     cs'))]
      (assoc pile :cards cards'))))

;;
;; State Actions
;;

(defn apply-selection
  [state selection]
  (let [{piles :piles} state]
    (reduce (fn [state {x :x, y :y, :as pile'}]
              (assoc-in state [:piles x y] pile'))
            state
            (map (partial pile-after-selection selection)
                 (mapcat vals (vals piles))))))

(defn start-selection-action
  [pos]
  (fn [state]
    (let [selection {:start pos, :stop pos}]
      (-> state
        (apply-selection selection)
        (assoc :selection selection)))))

(defn update-selection-action
  [pos]
  (fn [{:keys [piles selection] :as state}]
    (if selection
      (let [selection' (assoc selection :stop pos)]
        (-> state
          (assoc-in [:selection] selection'))))))

(defn stop-selection-action
  [_]
  (fn [{:keys [selection piles] :as state}]
    (-> state
      (apply-selection selection)
      (dissoc :selection))))

;;
;; Signal Graph
;;

(def initial-state
  (let [blank {:piles (sorted-map)}]
    (add-pile blank (repeatedly 7 forest))))

(def state-signal
  (let [drag-coords (sig/keep-when mouse/down? [0 0] mouse/position)
        dragging? (let [true-on-dragmove (sig/sample-on drag-coords (sig/constant true))]
                    (->> (sig/merge (sig/keep-if not false mouse/down?) true-on-dragmove)
                      sig/drop-repeats))
        start-drag (sig/keep-if identity true dragging?)
        stop-drag (sig/keep-if not false dragging?)
        drag-start-coords (sig/sample-on start-drag mouse/position)
        click-state-change (sig/drop-repeats mouse/down?)
        actions (sig/merge
                  (sig/lift start-selection-action drag-start-coords)
                  (sig/lift stop-selection-action stop-drag)
                  (sig/lift update-selection-action drag-coords)
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

(defn render-hud
  [state]
  (dom/div #js {:id "hud", :style #js {:position "relative"}}
           (dom/pre nil
                    (dom/b nil
                           (.stringify js/JSON (clj->js state) nil 2)))))

(defn render-state
  [state]
  (let [{selection :selection} state
        piles (mapcat vals (-> state :piles vals))]
    (dom/div #js {:id "dom-root"}
             (apply dom/div {:id "piles"} (map #(render-pile % selection) piles))
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

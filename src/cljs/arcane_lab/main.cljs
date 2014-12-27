(ns arcane-lab.main
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [>! <!]]
            [jamesmacaulay.zelkova.signal :as sig]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.time :as time]
            [jamesmacaulay.async-tools.core :as tools])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def card-width 222)
(def card-height 319)

(def gutter (quot card-width 8))
(def half-gutter (quot gutter 2))
(def pile-stride (quot card-height 9.25))

(def forest
  {:name "Forest"
   :img-src "http://magiccards.info/scans/en/po/205.jpg"
   :width card-width
   :height card-height
   :position [half-gutter half-gutter]})

(defn card-at
  [card pos]
  (assoc card :position pos))

(def initial-state {:selection nil
                    :cards (for [i (range 7)
                                 :let [dy (* i pile-stride)]]
                             (card-at forest [half-gutter (+ half-gutter dy)]))})

(defn start-selection-action
  [pos]
  (fn [state]
    (assoc state
           :selection {:start pos, :stop pos})))

(defn update-selection-action
  [pos]
  (fn [{:keys [selection] :as state}]
    (if-not selection
      (println "error: no selection found to update")
      (let [selection' (assoc selection :stop pos)]
        (-> state
          (assoc-in [:selection] selection')
          (dissoc :click?))))))

(defn clear-selection-on-click-action
  [mouse-down?]
  (fn [state]
    (cond
      mouse-down? (assoc state :click? true)
      (:click? state) (dissoc state :selection :click?)
      :otherwise state)))

(def state-signal
  (let [drag-coords (sig/keep-when mouse/down? [0 0] mouse/position)
        dragging? (let [true-on-dragmove (sig/sample-on drag-coords (sig/constant true))]
                    (->> (sig/merge (sig/keep-if not false mouse/down?) true-on-dragmove)
                      sig/drop-repeats))
        start-drag (sig/keep-if identity true dragging?)
        stop-drag (sig/keep-if not false dragging?)
        drag-start-coords (sig/sample-on start-drag mouse/position)
        drag-stop-coords (sig/sample-on stop-drag mouse/position)
        click-state-change (sig/drop-repeats mouse/down?)
        actions (sig/merge
                  (sig/lift start-selection-action drag-start-coords)
                  (sig/lift update-selection-action drag-coords)
                  (sig/lift clear-selection-on-click-action click-state-change)
                  (sig/constant identity))]
    (sig/reducep (fn [state action] (action state))
                 initial-state
                 actions)))

(def !app-state (sig/pipe-to-atom state-signal))

(defn render-selection
  [selection]
  (if selection
    (let [{[x1 y1] :start, [x2 y2] :stop} selection
          [left right] (sort [x1 x2])
          [top bottom] (sort [y1 y2])]
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
  (let [{:keys [name img-src width height], [x y] :position} card]
    (dom/div #js {:className "card"
                  :style #js {:position "absolute"
                              :left x
                              :top y}}
             (dom/img #js {:src img-src, :title name, :width width, :height height}))))

(defn render-state
  [state]
  (let [cards-top-to-bottom (sort-by #(get-in % [:position 1]) (:cards state))]
    (dom/div nil
             (apply dom/div nil (map render-card cards-top-to-bottom))
             (render-selection (:selection state))
             (dom/div #js {:id "hud", :style #js {:position "relative"}}
                      (dom/pre nil
                               (dom/b nil
                                      (.stringify js/JSON (clj->js state) nil 2)))))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  !app-state
  {:target (. js/document (getElementById "app"))})

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

(def initial-state {:cards []
                    :selection nil})


(defn start-selection-action
  [pos]
  (fn [state]
    (assoc state
           :cards []
           :selection {:start-coord pos})))

(def state-signal
  (let [drag-coords (sig/keep-when mouse/down? [0 0] mouse/position)
        dragging? (->> (sig/constant true)
                    (sig/sample-on drag-coords)
                    (sig/merge (sig/keep-if not false mouse/down?))
                    sig/drop-repeats)
        start-drag (sig/keep-if identity true dragging?)
        stop-drag (sig/keep-if not false dragging?)
        click-coords (sig/sample-on mouse/clicks mouse/position)
        drag-start-coords (sig/sample-on start-drag mouse/position)
        actions (sig/log
                  (sig/merge
                    (sig/constant identity)
                    (sig/lift start-selection-action drag-start-coords)))]
    (sig/reducep (fn [state action] (action state))
                 initial-state
                 actions)))

(def !app-state (sig/pipe-to-atom state-signal))

(defn render-state
  [state]
  (dom/pre nil (.stringify js/JSON (clj->js state) nil 2)))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  !app-state
  {:target (. js/document (getElementById "app"))})

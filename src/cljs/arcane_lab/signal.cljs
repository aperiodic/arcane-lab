(ns arcane-lab.signal
  (:require [arcane-lab.action :as action]
            [arcane-lab.constants :as c]
            [cljs.core.async :as async]
            [goog.events :as events]
            [jamesmacaulay.zelkova.keyboard :as keys]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.signal :as sig]))

(defn- on-key-code-down
  [code]
  (sig/keep-if identity true (sig/drop-repeats (keys/down? code))))

(defn- listen
  [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

(defn undo-channel
  [_ _]
  (listen (.getElementById js/document "undo-button")
          "click"
          (map (constantly true))))

(defn redo-channel
  [_ _]
  (listen (.getElementById js/document "redo-button")
          "click"
          (map (constantly true))))

(defn drag-drop-select-undo-redo
  [initial-state]
  (let [app-mouse-position (sig/map #(update-in % [1] - c/mouse-y-offset) mouse/position)
        drag-coords (sig/keep-when mouse/down? [0 0] app-mouse-position)
        dragging? (let [true-on-dragmove (sig/sample-on drag-coords (sig/constant true))]
                    (->> (sig/merge (sig/keep-if not false mouse/down?) true-on-dragmove)
                      sig/drop-repeats))
        start-drag (sig/keep-if identity true dragging?)
        stop-drag (sig/keep-if not false dragging?)
        drag-start-coords (sig/sample-on start-drag app-mouse-position)
        click-down (sig/keep-if identity true (sig/drop-repeats mouse/down?))
        click-down-coords (sig/sample-on click-down app-mouse-position)
        click-up (sig/keep-if not false (sig/drop-repeats mouse/down?))
        undo-button-down (sig/input false :undo-button undo-channel)
        redo-button-down (sig/input false :redo-button redo-channel)
        actions (sig/merge
                  (sig/map action/start-drag click-down-coords)
                  (sig/map action/start-selection-if-not-dragging drag-start-coords)
                  (sig/map action/update-selection-or-drag-destination drag-coords)
                  (sig/map action/stop-selection-or-drag stop-drag)
                  (sig/map action/stop-selection-or-drag click-up)
                  (sig/map action/rewind-state (on-key-code-down c/u-key-code))
                  (sig/map action/rewind-state undo-button-down)
                  (sig/map action/fast-forward-state (on-key-code-down c/r-key-code))
                  (sig/map action/fast-forward-state redo-button-down)
                  (sig/constant identity))]
    (sig/drop-repeats
      (sig/reductions (fn [state action] (action state))
                      initial-state
                      actions))))

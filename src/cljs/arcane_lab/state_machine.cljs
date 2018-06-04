(ns arcane-lab.state-machine
  (:require [arcane-lab.action :as action]
            [arcane-lab.constants :as c]
            [arcane-lab.piles :as piles]
            [arcane-lab.state :as state]
            [cypress.state-machine :as sm]))

;;
;; DOM -> Clojure
;;

(defn- table-coords
  [event]
  [(.-pageX event) (- (.-pageY event) c/mouse-y-offset)])

;;
;; Mode Selection Function
;;

(defn to-drag-or-selection
  "Dispatch function for mouse-down transitions out of the idle state;
  dispatches to :drag-started state if the mouse down is over a card, and
  :selecting otherwise."
  [app-state mouse-down]
  (let [[x y] (table-coords mouse-down)
        card-under-mouse (piles/card-under (:piles app-state) x y)
        {cards-under-mouse :cards} (piles/pile-under (:piles app-state) x y)
        multiple-cards-in-pile? (> (count cards-under-mouse) 1)
        first-card-under-mouse? (= (:id card-under-mouse)
                                   (:id (first cards-under-mouse)))]
    (cond
      (and card-under-mouse (:selected? card-under-mouse)) :drag-started
      (and first-card-under-mouse? multiple-cards-in-pile?) :scan-started
      card-under-mouse :drag-started
      :else-no-card :selecting)))

;;
;; Scan Mode
;;
;; Click on pile's logical bottom card (the one that has all the cards piled
;; on 'top' in relation to the pile's natural z-axis) and then drop it (w/out
;; dragging) to put it down on the pile's top (the card you can see all of
;; because it's not covered up by another). This lets you really quickly scan
;; through a pile to see all the cards by just clicking on the same place.
;;

(defn start-scan
  [app-state _ mouse-down]
  (let [[x y] (table-coords mouse-down)]
    (action/start-single-card-drag app-state x y)))

(defn drop-on-top
  [app-state _ mouse-up]
  (let [[x y] (table-coords mouse-up)]
    (action/drop-on-top app-state x y)))

;;
;; Drag Mode
;;
;; Could be dragging a selection or a lone card.
;;

(defn start-drag
  [app-state _ mouse-down]
  (let [[x y] (table-coords mouse-down)
        card-under-mouse (piles/card-under (:piles app-state) x y)]
      (cond
        (:selected? card-under-mouse) (action/start-selection-drag app-state x y)
        card-under-mouse (action/start-single-card-drag app-state x y)
        :otherwise (throw (js/Exception. "Bug in start-drag")))))

(defn return-cards
  [app-state _ _]
  (action/return-cards app-state))

(defn update-drag
  [app-state _ mouse-move]
  (let [[x y] (table-coords mouse-move)]
    (action/update-drag app-state x y)))

(defn drop-moved-cards
  [app-state _ mouse-up]
  (let [[x y] (table-coords mouse-up)]
    (action/drop-moved-cards app-state x y)))

;;
;; Selection Mode
;;

(defn start-selection
  [app-state _ mouse-down]
  (let [[x y] (table-coords mouse-down)]
    (action/start-selection app-state x y)))

(defn update-selection
  [app-state _ mouse-move]
  (let [[x y] (table-coords mouse-move)]
    (action/update-selection app-state x y)))

(defn apply-selection
  [app-state _ mouse-up]
  (let [[x y] (table-coords mouse-up)]
    (-> (action/update-selection app-state x y)
      (state/apply-selection (:selection app-state))
      (dissoc :selection))))

;;
;; UI State Machine for Cypress
;;

(defn start-drag-or-selection
  [state ui-state mouse-down]
  (case ui-state
    (:drag-started :scan-started) (start-drag state ui-state mouse-down)
    :selecting (start-selection state ui-state mouse-down)))

(def ui-states
  (-> (sm/blank-state-machine :idle)
    ;; :idle transitions        will return either :drag-started, :scan-started,
    ;;                        ↓ or :selecting
    (sm/add-transition :idle, to-drag-or-selection, :mouse-down
                       start-drag-or-selection)

    ;; :scan-started state
    (sm/add-transition :scan-started :idle :mouse-up drop-on-top)
    (sm/add-transition :scan-started :dragging :mouse-move update-drag)

    ;; :drag-started state
    (sm/add-transition :drag-started :idle :mouse-up return-cards)
    (sm/add-transition :drag-started :dragging :mouse-move update-drag)

    ;; :dragging state
    (sm/add-transition :dragging :idle :mouse-up drop-moved-cards)
    (sm/add-transition :dragging :dragging :mouse-move update-drag)

    ;; :selecting state
    (sm/add-transition :selecting :selecting :mouse-move update-selection)
    (sm/add-transition :selecting :idle :mouse-up apply-selection)))

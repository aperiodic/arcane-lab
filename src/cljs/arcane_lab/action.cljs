(ns arcane-lab.action
  (:require [arcane-lab.constants :as c]
            [arcane-lab.drag :as drag]
            [arcane-lab.geom :refer [within?]]
            [arcane-lab.history :as history]
            [arcane-lab.piles :as piles]
            [arcane-lab.state :as state]))

;;
;; New Actions, Used in Cypress UI State Machine
;;

(defn start-selection-drag
  [state x y]
  (let [piles-in-selection (filter #(some :selected? (:cards %))
                                     (state/state->piles state))
        selected-cards (filter :selected? (mapcat :cards piles-in-selection))
        drag-from (-> (first selected-cards)
                     (select-keys [:x :y]))
        [dx dy] (drag/drag-pile-pos x y)
        drag-pile (-> (piles/make-drag-pile selected-cards dx dy drag-from)
                    (assoc :started-at-mouse [x y]))
        state' (->> (reduce (fn [state {px :x, py :y, cards :cards}]
                              (let [cards' (remove :selected? cards)]
                                (if (empty? cards')
                                  (state/remove-pile state px py)
                                  (state/add-pile state (piles/make-pile cards' px py)))))
                            state
                            piles-in-selection)
                 (state/map-cards #(assoc % :dropped? false)))]
    (assoc state'
           :drag drag-pile
           :drag-target (drag/drag-target dx dy (:piles state')))))

(defn start-single-card-drag
  [{:keys [piles] :as state} x y]
  (let [{px :x, py :y :as pile} (piles/pile-under piles x y)
        {target-card-id :id, :as target-card} (piles/card-under piles x y)
        pile-cards' (remove #(= (:id %) target-card-id) (:cards pile))
        first-card? (= (-> pile :cards (nth 0) :id) target-card-id)
        drag-from (select-keys target-card [:x :y])
        [dx dy] (drag/drag-pile-pos x y)
        drag (piles/make-drag-pile [target-card] dx dy drag-from first-card?)
        state' (->> (if (empty? pile-cards')
                      (state/remove-pile state px py)
                      (state/add-pile state (piles/make-pile pile-cards' px py)))
                 (state/map-cards #(assoc % :dropped? false)))]
    (assoc state' :drag drag :drag-target (drag/drag-target dx dy (:piles state')))))

(defn update-drag
  [state x y]
  (let [max-pile-x (or (:max-pile-x state)
                       (piles/max-pile-x (:piles state)))
        ;; should be able to drag pile all the way over to new column
        max-drag-x (+ max-pile-x (* 2 c/card-width) c/half-card-width c/gutter)]
    (state/update-drag state (min max-drag-x x) y)))

(defn rehash-state
  "Call this when new piles are added to the state (a.k.a. cache invalidation).
  Recalculates all the decision variables and trigger boundaries that are
  derived from the state (like the 'drop zones' that help help optimize the
  mouse-move handler overhead)."
  [state]
  (let [state'  (-> state
                  state/add-max-pile-x
                  state/add-selection-triggers
                  state/add-drop-zones
                  history/add-new-state!)]
    (history/save-state! state')
    state'))

;; TODO: main thing left to do is fill in the drop-* functions with the right
;; parts from the old stop-selection-or-drag

(defn- drop-cards
  "Insert the drag's cards into or create a new pile at target-x target-y, with
  target-pile-index an integer only in the insertion case.  Does *not* do any
  modification of the target x & y values, they must refer to the exact pile
  location. Do all the decision-making elsewhere.

  Returns the state after the drop has been completed and all derived values
  re-calculated."
  [state drag target-x target-y target-pile-index]
  (let [ti target-pile-index
        {old-cards :cards} (state/get-pile state target-x target-y)
        raw-drag-cards (:cards drag)
        highlight? (and (not (:selected? (first raw-drag-cards)))
                        (not= ti :below-pile)
                        ;; this part ugly
                        (or (= ti :no-pile)
                            (and (number? ti)
                                 (< ti (count old-cards))))
                        (pos? (count old-cards)))
        drag-cards (map #(assoc % :dropped? highlight?) raw-drag-cards)
        missed-pile? (or (= ti :below-pile) (= ti :above-pile))
        new-cards (cond
                    (= ti :no-pile) drag-cards
                    missed-pile? (concat old-cards drag-cards)
                    :else (concat (take ti old-cards)
                                  drag-cards
                                  (drop ti old-cards)))]
    (-> state
      (state/add-pile (piles/make-pile new-cards target-x target-y))
      (update-in [:piles] piles/rejigger-rows)
      (dissoc :drag :drag-target)
      rehash-state)))

(defn drop-on-top
  ;; this is the drop that enables the scanning behavior
  [state target-x target-y]
  (let [{tx :x, ty :y, :as target-pile} (piles/pile-under (:piles state)
                                                          target-x target-y)]
    (drop-cards state (:drag state) tx ty :above-pile)))

(defn return-cards
  [state]
  (let [[dx_0 dy_0] (get-in state [:drag :dragged-from])
        [tx ty ti] (drag/drag-target dx_0 dy_0 (:piles state))]
    (drop-cards state (:drag state) tx ty ti)))

(defn drop-moved-cards
  [state x y]
  (let [{dx :x, dy :y, :as drag} (:drag state)
        [tx ty ti] (drag/drag-target dx dy (:piles state))]
    (drop-cards state drag tx ty ti)))

(defn start-selection
  [state x y]
  (let [selection {:start [x y] , :stop [x y] , :count 0}]
    (-> state
      (state/apply-selection selection)
      (assoc :selection selection))))

(defn update-selection
  [state x y]
  (let [max-pile-x (or (:max-pile-x state) (piles/max-pile-x (:piles state)))
        max-selection-x (+ max-pile-x c/card-width c/gutter)]
    (state/update-selection state (min max-selection-x x) y)))


;;
;; Old Actions, Used in Zelkova Signal Graph
;;

(defn start-drag
  "Start a drag if:
    * there are selected cards and the click is on a pile with selected cards
      (but not above the selected cards in that pile);
    * there the click is on some unselected card (start dragging
      just that card, regardless of whether or not there's a selection).
  Returns the state with or without a newly-started drag, depending on if the
  above criteria are satisfied. "
  [pos]
  (fn [{:keys [piles] :as state}]
    (let [[x y] pos
          card-under-mouse (piles/card-under piles x y)]
      (cond
        (:selected? card-under-mouse) (start-selection-drag state x y)
        card-under-mouse (start-single-card-drag state x y)
        :otherwise (dissoc state :drag)))))

(defn start-selection-if-not-dragging
  [pos]
  (fn [state]
    (if (:drag state)
      state
      (let [selection {:start pos, :stop pos, :count 0}]
        (-> state
          (state/apply-selection selection)
          (assoc :selection selection))))))

(defn update-selection-or-drag-destination
  [pos]
  (fn [state]
    (let [[x y] pos]
      (cond
        (:drag state) (update-drag state x y)
        (:selection state) (update-selection state x y)
        :else state))))

(defn stop-selection-or-drag
  [_]
  (fn [{:keys [selection drag piles] :as state}]
    (cond
      selection (-> state
                  (state/apply-selection selection)
                  (dissoc :selection))
      drag (let [{dx :x, dy :y, d-cs :cards} drag
                 ;; drag target shouldn't know about whether the drag has moved,
                 ;; right? I think I want to handle that at the action level
                 [tx ty ti] (drag/drag-target dx dy piles)
                 {old-cards :cards} (state/get-pile state tx ty)
                 highlight? (and (not (:selected? (first d-cs)))
                                 (not= ti :below-pile)
                                 (or (= ti :no-pile)
                                     (< ti (count old-cards)))
                                 (pos? (count old-cards)))
                 drag-cards (map #(assoc % :dropped? highlight?) d-cs)
                 new-cards (cond
                            (= ti :no-pile) drag-cards
                            (or (= ti :below-pile) (= ti :above-pile)) (concat old-cards drag-cards)
                            :else (concat (take ti old-cards) drag-cards (drop ti old-cards)))]
             (-> state
               (state/add-pile (piles/make-pile new-cards tx ty))
               (update-in [:piles] piles/rejigger-rows)
               (dissoc :drag :drag-target)
               state/add-max-pile-x
               state/add-selection-triggers
               state/add-drop-zones
               history/add-new-state!
               (doto history/save-state!)))
      :otherwise state)))

;;
;; State History Manipulation
;;

(defn rewind-state!
  [should-rewind?]
  (if should-rewind?
    (fn [current]
      (let [previous (history/rewind! current)]
        (history/save-state! previous)
        previous))
    identity))

(defn fast-forward-state!
  [_]
  (fn [current]
    (let [nxt (history/fast-forward! current)]
      (history/save-state! nxt)
      nxt)))

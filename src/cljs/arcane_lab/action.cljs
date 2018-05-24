(ns arcane-lab.action
  (:require [arcane-lab.constants :as c]
            [arcane-lab.drag :as drag]
            [arcane-lab.geom :refer [within?]]
            [arcane-lab.history :as history]
            [arcane-lab.piles :as piles]
            [arcane-lab.state :as state]))

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
          piles-in-selection (filter #(some :selected? (:cards %))
                                     (state/state->piles state))
          card-under-mouse (piles/card-under piles x y)
          extant-selection-drag? (:selected? card-under-mouse)
          [dx dy] (drag/drag-pile-pos x y)]

      (cond
        extant-selection-drag?
        (let [selected-cards (filter :selected? (mapcat :cards piles-in-selection))
              drag-start (-> (first selected-cards)
                           (select-keys [:x :y]))
              drag-pile (piles/make-drag-pile selected-cards dx dy drag-start)
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
                 :drag-target (drag/drag-target drag-pile (:piles state'))))

        card-under-mouse ;; make drag pile w/only current card
        (let [{px :x, py :y :as pile} (piles/pile-under piles x y)
              dragged-card-id (:id card-under-mouse)
              pile-cards' (remove #(= (:id %) dragged-card-id) (:cards pile))
              first-card? (= (-> pile :cards (nth 0) :id) dragged-card-id)
              drag-start (select-keys card-under-mouse [:x :y])
              state' (->> (if (empty? pile-cards')
                            (state/remove-pile state px py)
                            (state/add-pile state (piles/make-pile pile-cards' px py)))
                       (state/map-cards #(assoc % :dropped? false)))
              drag (piles/make-drag-pile [card-under-mouse] dx dy drag-start first-card?)]
          (assoc state' :drag drag :drag-target (drag/drag-target drag (:piles state'))))

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
  (fn [{:keys [drag selection piles] :as state}]
    (let [max-pile-x (or (:max-pile-x state) (piles/max-pile-x piles))
          max-x (+ max-pile-x c/card-width c/gutter
                   (if drag (+ c/card-width c/half-card-width) 0))
          x (min max-x (nth pos 0))
          y (nth pos 1)]
      (cond
        drag (state/update-drag state x y)
        selection (state/update-selection state x y)
        :else state))))

(defn stop-selection-or-drag
  [_]
  (fn [{:keys [selection drag piles] :as state}]
    (cond
      selection (-> state
                  (state/apply-selection selection)
                  (dissoc :selection))
      drag (let [{dx :x, dy :y, d-cs :cards} drag
                 [tx ty ti] (drag/drag-target drag piles)
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

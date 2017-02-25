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
    (let [piles-in-selection (filter #(some :selected? (:cards %))
                                     (state/state->piles state))
          [x y] pos
          extant-selection-drag? (loop [ps piles-in-selection]
                                   (if-let [{l :x, pt :y, cards :cards, :as p} (first ps)]
                                     (let [r (+ l c/card-width)
                                           t (-> (filter :selected? cards) first :y)
                                           b (+ pt (piles/pile-height p))]
                                       (if (within? l r t b x y)
                                         true
                                         (recur (next ps))))
                                     ;; else (no more piles)
                                     false))
          card-under-mouse (piles/card-under piles x y)
          [dx dy] (drag/drag-pile-pos x y)
          drag-target (drag/drag-target {:x dx, :y dy} piles)]

      (cond
        extant-selection-drag?
        (let [[drag-pile-x drag-pile-y] (drag/drag-pile-pos x y)
              selected-cards (filter :selected?
                                     (mapcat :cards piles-in-selection))
              drag-pile (piles/make-drag-pile selected-cards
                                              drag-pile-x, drag-pile-y)]
          (-> (reduce (fn [state {px :x, py :y, cards :cards}]
                        (let [cards' (remove :selected? cards)]
                          (if (empty? cards')
                            (state/remove-pile state px py)
                            (state/add-pile state
                                            (piles/make-pile cards' px py)))))
                      state
                      piles-in-selection)
            (assoc :drag drag-pile
                   :drag-target drag-target)))

        card-under-mouse ;; make drag pile w/only current card
        (let [[dpx dpy] (drag/drag-pile-pos x y)
              {px :x, py :y :as pile} (piles/pile-under piles x y)
              pile-cards' (remove #(= (:id %) (:id card-under-mouse))
                                  (:cards pile))]
          (-> (if (empty? pile-cards')
                (state/remove-pile state px py)
                (state/add-pile state (piles/make-pile pile-cards' px py)))
            (assoc :drag (piles/make-drag-pile [card-under-mouse] dpx dpy)
                   :drag-target drag-target)))

        :otherwise (dissoc state :drag)))))

(defn start-selection-if-not-dragging
  [pos]
  (fn [state]
    (if (:drag state)
      state
      (let [selection {:start pos, :stop pos}]
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
      drag (let [[tx ty] (drag/drag-target drag piles)
                 new-pile (if-let [{old-cards :cards} (state/get-pile state tx ty)]
                            (piles/make-pile (concat old-cards (:cards drag))
                                             tx, ty)
                            (piles/make-pile (:cards drag) tx ty))]
             (-> state
               (state/add-pile new-pile)
               (update-in [:piles] piles/rejigger-rows)
               (dissoc :drag :drag-target)
               state/add-max-pile-x
               state/add-selection-triggers
               state/add-drop-zones
               history/add-to-history-if-new!))
      :otherwise state)))

(defn rewind-state
  [should-rewind?]
  (if should-rewind?
    (fn [current] (history/rewind! current))
    identity))

(defn skip-ahead-state
  [_]
  (fn [current] (history/skip-ahead! current)))
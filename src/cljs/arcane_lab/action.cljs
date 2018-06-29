(ns arcane-lab.action
  (:require [arcane-lab.constants :as c]
            [arcane-lab.drag :as drag]
            [arcane-lab.geom :refer [within?]]
            [arcane-lab.history :as history]
            [arcane-lab.piles :as piles]
            [arcane-lab.state :as state]))

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
    (assoc state' :drag drag)))

(defn update-drag
  [state x y]
  (let [max-pile-x (or (:max-pile-x state)
                       (piles/max-pile-x (:piles state)))
        ;; should be able to drag pile all the way over to a new blank column
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

;; this is the drop that enables the scanning behavior. it places the card on
;; the 'logical' top of the pile (where you can see its entire face), which is
;; the 'screen' bottom.
(defn drop-on-top
  [state target-x target-y]
  (let [{tx :x, ty :y, :as target-pile} (piles/pile-under (:piles state)
                                                          target-x target-y)]
    (drop-cards state (:drag state) tx ty :above-pile)))

(defn return-cards
  [state]
  (let [{dx_0 :x, dy_0 :y} (get-in state [:drag :dragged-from])
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

(defn rewind!
  [current]
  (let [previous (history/rewind! current)]
    (history/save-state! previous)
    previous))

(defn fast-forward!
  [current]
  (let [nxt (history/fast-forward! current)]
    (history/save-state! nxt)
    nxt))

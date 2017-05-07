(ns arcane-lab.state
  (:require [arcane-lab.constants :as c]
            [arcane-lab.drag :as drag]
            [arcane-lab.geom :as geom]
            [arcane-lab.piles :as piles]
            [arcane-lab.util :refer [binary-search]]))

(defn state->piles
  [state]
  (->> (:piles state)
    vals
    (mapcat vals)))

(defn state->cards
  "Given a state with :piles, return all the cards in all the piles (does not
  include dragged cards)."
  [state]
  (->> (vals (:piles state))
    (mapcat vals)
    (mapcat :cards)))

(defn map-piles
  [f state]
  (reduce (fn [state {x :x, y :y, :as pile'}]
            (assoc-in state [:piles y x] pile'))
          state
          (map f (state->piles state))))

(defn map-cards
  [f state]
  (map-piles #(piles/map-cards f %) state))

(defn get-pile
  [state x y]
  (get-in state [:piles y x]))

(defn add-pile
  [state pile]
  (let [{:keys [x y]} pile]
    (-> state
       (update-in [:piles y] (fnil identity (sorted-map)))
       (assoc-in [:piles y x] pile))))

(defn remove-pile
  [state x y]
  (let [row (get-in state [:piles y])]
    (if (and (contains? row x) (= (count row) 1))
      (update-in state [:piles] dissoc y)
      (update-in state [:piles y] dissoc x))))

(defn apply-selection
  [state selection]
  (let [{piles :piles} state]
    (if-not selection
      state
      (map-piles (partial piles/pile-after-selection selection) state))))

(defn update-selection
  [state x' y']
  (let [[x y] (get-in state [:selection :stop])
        {xs :vertical, ys :horizontal} (:selection-triggers state)
        update? (or (geom/any-lines-between? y y' ys)
                    (geom/any-lines-between? x x' xs))
        state' (assoc-in state [:selection :stop] [x' y'])]
    (if-not update?
      state'
      (apply-selection state' (:selection state')))))

(defn update-drag
  [state x' y']
  (let [{dx :x, dy :y} (:drag state)
        [x y] (drag/mouse-pos dx dy)
        {xs :vertical, ys :horizontal} (:drag-triggers state)
        update? (or (geom/any-lines-between? y y' ys)
                    (geom/any-lines-between? x x' xs)
                    (false? (get-in state [:drag :moved?])))
        [dx' dy'] (drag/drag-pile-pos x' y')
        state' (update state :drag piles/move-drag-pile-to dx' dy')]
    (if-not update?
      state'
      (assoc state' :drag-target (drag/drag-target (:drag state')
                                                   (:piles state'))))))

;;
;; Cached State Properties
;;

(defn add-max-pile-x
  [state]
  (assoc state :max-pile-x (piles/max-pile-x (:piles state))))

(defn add-selection-triggers
  [state]
  (assoc state :selection-triggers (piles/selection-check-boundaries (:piles state))))

(defn add-drop-zones
  [state]
  (assoc state :drag-triggers (piles/drop-zones (:piles state))))

(defn add-dfcs
  [state]
  (assoc state :dfcs (filter :dfc? (state->cards state))))


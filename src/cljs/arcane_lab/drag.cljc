(ns arcane-lab.drag
  (:require [arcane-lab.constants :as c]
            [arcane-lab.piles :as piles]
            [dlp.useful.math :refer [half mean]]))

(defn drag-pile-pos
  "Given the mouse's x & y coordinates, return the position of the drag pile
  such that the mouse is in the center."
  [x y]
  [(- x c/drag-x-offset)
   (- y c/drag-y-offset)])

(defn mouse-pos
  [drag-x drag-y]
  [(+ drag-x c/drag-x-offset)
   (+ drag-y c/drag-y-offset)])

(comment
  ;; this is the 'undo drag' logic from drag-target that should now be lifted up
  ;; into the state machine graph

  (let [undo-drag? (and (false? (:moved? drag))
                        (false? (:first-card-picked? drag)))
        {dx :x, dy :y} (if undo-drag? (:cards-orig-pos drag) drag) ])
  )

(defn drag-target
  ;; need to finish rewriting this to take the drag x and y, not do any of the
  ;; undo-drag? nonsense (that should be up to the state machine)
  [drag-x drag-y piles]
  (let [dy drag-y
        [x y] (mouse-pos drag-x drag-y)
        col-index (quot (- x c/half-gutter) c/pile-spacing)
        row-ys (keys piles)
        row-count (count piles)
        row-spacings (vec (piles/row-spacings row-ys))
        [before-and-on after] (split-with #(<= % y) row-ys)
        first-row-y c/half-gutter
        row-y (or (last before-and-on) first-row-y)
        curr-row-height (cond
                          (= row-count 1) (piles/row-i-height piles 0)
                          (< y first-row-y) (piles/row-i-height piles 0)
                          (not (empty? after)) (nth row-spacings (dec (count before-and-on)))
                          :otherwise (piles/row-height (get piles (last row-ys))))
        left-col (-> (* col-index c/pile-spacing)
                   (+ c/half-gutter))
        right-col (-> (* (inc col-index) c/pile-spacing)
                    (+ c/half-gutter))
        left-col-center (+ left-col c/half-card-width)
        right-col-center (+ right-col c/half-card-width)
        hovered-pile (piles/pile-at piles left-col row-y)
        hov-height (max (or (:height hovered-pile) 0) c/card-height)
        tx (if (<= x (mean left-col-center right-col-center)) left-col right-col)
        ty (if (<= y (min (+ row-y hov-height (* 0.66 c/card-height))
                          (+ row-y curr-row-height)))
             row-y (+ row-y curr-row-height))
        target-pile (piles/pile-at piles tx ty)]
    (if-not target-pile
      [tx ty :no-pile]
      (let [bottom-of-last-card (+ ty (:height target-pile))
            top-of-last-card (- bottom-of-last-card (- c/card-height c/pile-stride))]
        (cond
          (>= dy bottom-of-last-card) [tx ty :above-pile]
          (>= dy top-of-last-card) [tx ty (count (:cards target-pile))]
          (<= dy (- ty (half c/pile-stride))) [tx ty :below-pile]
          :else [tx ty (-> (- dy ty) (/ c/pile-stride) Math/round)])))))

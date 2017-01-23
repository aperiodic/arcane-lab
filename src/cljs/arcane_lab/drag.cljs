(ns arcane-lab.drag
  (:require [arcane-lab.constants :as c]
            [arcane-lab.piles :as piles]))

(def drag-x-offset (/ c/card-width 2))
(def drag-y-offset (-> (* 0.4 c/card-height) int))

(defn drag-pile-pos
  "Given the mouse's x & y coordinates, return the position of the drag pile
  such that the mouse is in the center."
  [x y]
  [(- x drag-x-offset)
   (- y drag-y-offset)])

(defn mouse-pos
  [drag-x drag-y]
  [(+ drag-x drag-x-offset)
   (+ drag-y drag-y-offset)])

(defn distance-squared-to
  ([p] (let [{x :x y :y} p] (distance-squared-to x y)))
  ([x y]
   (fn [[cx cy]]
     (let [dx (- x (+ cx c/half-card-width))
           dy (- y (+ cy c/half-card-height))]
       (+ (* dx dx) (* dy dy))))))

(defn drag-target
  [drag piles]
  ;; want to find the nearest drop position for the drag, which means:
  ;;   nearest pile or place where new pile could be inserted
  (if drag
    (let [[x y] (mouse-pos (:x drag) (:y drag))
          col-index (quot (- x c/half-gutter) c/pile-spacing)
          left-col (-> (* col-index c/pile-spacing)
                     (+ c/half-gutter))
          right-col (-> (* (inc col-index) c/pile-spacing)
                      (+ c/half-gutter))
          rows (vals piles)
          all-piles (mapcat vals rows)
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
          candidates (for [cx [left-col right-col]
                           cy [row-y (+ row-y curr-row-height)]]
                       [cx cy])]
      (first (sort-by (distance-squared-to x y) candidates)))))


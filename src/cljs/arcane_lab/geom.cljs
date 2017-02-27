(ns arcane-lab.geom
  (:require [arcane-lab.util :refer [binary-search]]))

(defn within?
  "Returns true if the point at (x,y) is within the box defined by left, right,
  top, and bottom."
  [left right top bottom x y]
  (and (<= x right)  ; the point is to the left of the box's right edge
       (>= x left)   ; the point is to the right of the box's left edge
       (<= y bottom) ; the point is above the bottom of the box
       (>= y top)))  ; the point is below the top of the box

(defn between?
  [lo hi x]
  (and (>= x lo)
       (<= x hi)))

(defn any-lines-between?
  [c0 c1 sorted-lines]
  ;; the 0.1 is a trick to make the two passed coordinates never exactly = any
  ;; of the sorted lines' coordinates
  (not= (binary-search sorted-lines (+ 0.1 c0))
        (binary-search sorted-lines (+ 0.1 c1))))


(ns arcane-lab.geom)

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


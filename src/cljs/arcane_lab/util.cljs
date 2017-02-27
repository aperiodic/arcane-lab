(ns arcane-lab.util)

(defn half
  [x]
  (/ x 2))

(defn mean
  [x y]
  (-> (+ x y) (/ 2)))

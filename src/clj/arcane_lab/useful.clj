(ns arcane-lab.useful
  (:require [useful.fn :as ufn]))

(defn any?
  [& preds]
  (comp
    boolean
    (apply ufn/any preds)))

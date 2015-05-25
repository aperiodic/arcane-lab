(ns arcane-lab.bucket
  (:refer-clojure :exclude [get set]))

(defprotocol Bucket
  (bget [this code])
  (bset [this code value]))

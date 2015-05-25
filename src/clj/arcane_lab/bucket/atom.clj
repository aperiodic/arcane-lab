(ns arcane-lab.bucket.atom
  (:refer-clojure :exclude [get])
  (:require [arcane-lab.bucket :as bucket]))

(defrecord AtomBucket
  [atm]

  bucket/Bucket
  (bget [_ code] (clojure.core/get @atm code))
  (bset [_ code value] (swap! atm assoc code value)))

(defn init
  []
  (AtomBucket. (atom {})))

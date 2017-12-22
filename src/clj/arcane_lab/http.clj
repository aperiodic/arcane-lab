(ns arcane-lab.http
  (:require [clojure.string :as str]))

(def negative-values
  (let [negatives #{"n" "no" "f" "false" "0"}]
    (->> (map str/upper-case negatives)
      (into negatives))))

(defn param->bool
  [parameter-value]
  (and (boolean parameter-value)
       ;; if it's set and not in the negatives list, default to truthy
       (not (contains? negative-values parameter-value))))

(defn parse-int
  [int-str]
  (Long/parseLong int-str))

(ns arcane-lab.transform
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn deprocess-card
  "The inverse of `process-card`"
  [card]
  (let [key->words-ish #(-> % name (str/replace "-" " "))]
    (-> card
      (update-in [:colors] (partial mapv key->words-ish))
      (update-in [:rarity] key->words-ish))))

(defn serialize-sets
  [magic-sets]
  (let [raw-sets (into {} (for [[code magic-set] magic-sets]
                            [code (update-in magic-set [:cards] (partial map deprocess-card))]))]
    (json/encode magic-sets)))

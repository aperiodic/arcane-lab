(ns arcane-lab.transform
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn to-mtgjson-card
  "The inverse of `cards/keywordize-and-categorize`"
  [card]
  (let [key->words-ish #(-> % name (str/replace "-" " "))]
    (-> card
      (update :colors (partial mapv key->words-ish))
      (update :rarity key->words-ish)
      (update :number str)
      (dissoc :dfc? :composite?))))

(defn serialize-sets
  [magic-sets]
  (let [raw-sets (into {} (for [[code magic-set] magic-sets]
                            [code (update-in magic-set [:cards] (partial map to-mtgjson-card))]))]
    (json/encode magic-sets)))

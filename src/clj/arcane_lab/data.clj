(ns arcane-lab.data
  "Functions for manipulating the JSON data from the MtGJSON project, e.g. for
  merging a standalone set file into all-sets file."
  (:require [arcane-lab.sets :as sets]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(def CARDS_BY_SET_FILENAME "cards-by-set.json")
(def CARDS_BY_SET_FILEPATH (str "resources/" CARDS_BY_SET_FILENAME))

(defn parse-json-resource
  [resource-name]
  (-> (io/resource resource-name)
    slurp
    (json/decode true)))

(defn save-as-json
  [thing path]
  (let [json (json/encode thing true)
        out (java.io.File. path)]
    (spit out json)))

(defn set-card->all-sets-card
  [card]
  (dissoc card
          :foreignNames :originalText :originalType :printings))

(defn all-sets []
  (parse-json-resource CARDS_BY_SET_FILENAME))

(defn eldritch-moon []
  (-> (parse-json-resource "EMN.json")
    (update :cards (partial map set-card->all-sets-card))))

(defn map-over-sets
  [f all-sets]
  (into {} (for [[code mtg-set] all-sets]
             [code (f mtg-set)])))

(defn filter-sets
  [f all-sets]
  (into {} (for [[code mtg-set] all-sets
                 :when (f mtg-set)]
             [code mtg-set])))

(defn map-over-cards
  [f all-sets]
  (map-over-sets (fn [s] (update s :cards #(mapv f %)))
                 all-sets))

(defn trim-set
  [mtg-set]
  (dissoc mtg-set :tokens :translations))

(defn trim-card
  [card]
  (dissoc card
          :flavorText, :foreignData, :legalities, :prices, :printings
          :purchaseUrls, :rulings))

(defn trim-all-sets
  [all-sets]
  (->> all-sets
    (filter-sets :boosterV3)
    (map-over-sets trim-set)
    (map-over-cards trim-card)))

(defn -main
  [& _]
  (let [trimmed-sets (trim-all-sets (all-sets))]
    (spit CARDS_BY_SET_FILEPATH (json/encode trimmed-sets))))

(comment
  (def emn (eldritch-moon))


  (let [processed-emn (-> (eldritch-moon)
                        (update :cards #(map set-card->all-sets-card %)))]
    (-> (all-sets)
      (assoc :EMN processed-emn)
      (save-as-json "resources/cards-by-set.json")))

  (-> (all-sets)
    :EMN
    :cards
    (->> (filter meld-card?)
      (map #(select-keys % [:name :number])))
    pprint
    )

  )

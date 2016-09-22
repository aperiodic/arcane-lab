(ns arcane-lab.data
  "Functions for manipulating the JSON data from the MtGJSON project, e.g. for
  merging a standalone set file into all-sets file."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

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
  (parse-json-resource "cards-by-set.json"))

(defn eldritch-moon []
  (-> (parse-json-resource "EMN.json")
    (update :cards (partial map set-card->all-sets-card))))

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

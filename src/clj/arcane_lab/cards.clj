(ns arcane-lab.cards
  (:require bigml.sampling.simple
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [cheshire.core :as json]))

(defn rarity-key
  [rarity]
  (-> rarity
    str/lower-case
    (str/replace " " "-")
    keyword))

(def rare-or-mythic #{:rare :mythic-rare})

(def all-sets
  (-> (io/resource "cards-by-set.json")
    io/file
    slurp
    (json/decode true)))

(defn process-set
  "Pre-process a set to:
    1) group cards by rarity
    2) change string values in booster specs to keywords"
  [set]
  (let [keywordize-string (fn [x]
                            (if (string? x)
                              (rarity-key x)
                              x))]
    (-> set
      (update-in [:cards] #(group-by (comp rarity-key :rarity) %))
      (update-in [:booster] (partial postwalk keywordize-string)))))

(def booster-sets
  (into {} (for [[code set] all-sets :when (contains? set :booster)]
             [code (process-set set)])))

(def basic-lands
  (->> (get booster-sets :CHK)
    :cards
    :basic-land))

(defn sample
  ([from seed]
   (bigml.sampling.simple/sample from :seed seed :generator :twister))
  ([from seed weights]
   (bigml.sampling.simple/sample from
                                 :weigh weights
                                 :seed seed
                                 :generator :twister)))

(def special-land-sampler
  {})

(defn sample-cards-by-rarity
  [set rarity amount seed]
  (let [set-code (keyword (:code set))]
    (if (= rarity :land)
      (if-let [sampler (special-land-sampler set-code)]
        (->> (sampler seed) (take amount))
        (->> (sample basic-lands seed) (take amount)))
      (let [rarity (if (and (vector? rarity)
                            (= (clojure.core/set rarity) rare-or-mythic))
                     (first (sample rare-or-mythic seed {:rare 7 :mythic-rare 1}))
                     rarity)
            cards (rarity (:cards set))]
        (->> (sample cards seed)
          (take amount))))))

(defn booster
  ([set-code] (booster set-code (rand-int Integer/MAX_VALUE)))
  ([set-code seed]
   (if-let [{:keys [cards] :as set} (get booster-sets set-code)]
     (let [booster-spec (remove #{"marketing"} (:booster set))]
       (mapcat (fn [rarity-section]
                 (let [rarity (first rarity-section)
                       amount (count rarity-section)]
                   (sample-cards-by-rarity set rarity amount seed)))
               (partition-by identity booster-spec))))))

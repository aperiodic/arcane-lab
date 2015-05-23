(ns arcane-lab.cards
  (:require [arcane-lab.utils :refer [rand-seed sample words->key]]
            [bigml.sampling.simple]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]]
            [cheshire.core :as json]))

;;
;; Definitions
;;

(def rare-or-mythic #{:rare :mythic-rare})
(def rare-weights {:rare 8, :mythic-rare 1})

(def basic-names #{"Plains" "Island" "Swamp" "Mountain" "Forest"})

(def refuge-names #{"Bloodfell Caves"
                    "Blossoming Sands"
                    "Dismal Backwater"
                    "Jungle Hollow"
                    "Rugged Highlands"
                    "Scoured Barrens"
                    "Swiftwater Cliffs"
                    "Thornwood Falls"
                    "Tranquil Cove"
                    "Wind-Scarred Crag"})

(def ally-fetch-names #{"Bloodstained Mire"
                        "Flooded Strand"
                        "Polluted Delta"
                        "Windswept Heath"
                        "Wooded Foothills"})

;;
;; Card & Set Processing
;;

(def special-set-processor
  {:FRF (fn [frf]
          ;; Fate Reforged contains the refuges also printed in Khans, but they
          ;; should only show up in the land slot, never in the comons.
          (let [remove-refuges (partial remove #(contains? refuge-names (:name %)))]
            (-> frf
              (update-in [:cards :common] remove-refuges))))})

(defn process-card
  "Pre-process a card to:
    1 - keywordize colors
    2 - keywordize rarity with words->key"
  [card]
  (-> card
    (update-in [:colors] (partial mapv words->key))
    (update-in [:rarity] words->key)))

(defn process-booster-set
  "Pre-process a set to:
    1 - group cards by rarity
    2 - change string values in booster specs to keywords"
  [set]
  (let [keywordize-string (fn [x]
                            (if (string? x)
                              (words->key x)
                              x))
        code (-> set :code keyword)
        special-processor (special-set-processor code identity)]
    (-> set
      (update-in [:cards] #(group-by :rarity %))
      (update-in [:booster] (partial postwalk keywordize-string))
      special-processor)))

;;
;; Load & Process Sets
;;

(def all-sets
  (let [raw-sets (-> (io/resource "cards-by-set.json")
                   slurp
                   (json/decode true))]
    (into {} (for [[code set] raw-sets]
               [code (update-in set [:cards] (partial map process-card))]))))

(def booster-sets
  (into {} (for [[code set] all-sets :when (contains? set :booster)]
             [code (process-booster-set set)])))

;;
;; Card Search
;;


(defn printing-in-set
  [nombre magic-set]
  (let [cards (:cards magic-set)
        cardseq (cond-> cards
                  (map? cards)
                  (->>
                    vals
                    (mapcat identity)))]
    (->> cardseq
      (filter #(= (:name %) nombre))
      (map #(assoc % :set (:code magic-set)))
      first)))

(defn printings
  [nombre]
  (->> (map (partial printing-in-set nombre) (vals all-sets))
    (keep identity)))

;;
;; Special Case Samplers
;;

(def special-land-sampler
  {:FRF
   ;; Fate Reforged may contain a refuge or a Khans of Tarkir fetchland in its
   ;; land slot. The specific numbers are just guesses as to how many of each
   ;; kind are actually printed on the land sheet.
   (let [ktk-fetches (filter #(contains? ally-fetch-names (:name %))
                             (get-in booster-sets [:KTK :cards :rare]))
         frf-refuges (filter #(contains? refuge-names (:name %))
                             (get-in all-sets [:FRF :cards]))
         land-cards (concat ktk-fetches frf-refuges)
         land-sheet (concat (take 105 (cycle refuge-names))
                            (take 5 (cycle ally-fetch-names)))]
     (fn [seed]
       (let [sampled-name (->> (sample land-sheet seed) (drop 55) first)]
         (-> (filter #(= (:name %) sampled-name) land-cards)
           first))))})

;;
;; Booster Sampling
;;

(def basic-land-cards
  (->> (get booster-sets :CHK)
    :cards
    :basic-land))

(defn sample-cards-by-rarity
  [set rarity amount seed]
  (let [set-code (keyword (:code set))]
    (if (= rarity :land)
      (if-let [sampler (special-land-sampler set-code)]
        [(sampler seed)]
        (->> (sample basic-land-cards seed) (take amount)))
      (let [rarity (if (and (vector? rarity)
                            (= (clojure.core/set rarity) rare-or-mythic))
                     (-> (bigml.sampling.simple/sample
                            rare-or-mythic
                            :seed seed :replace true :weigh rare-weights :generator :twister)
                       (nth 10))
                     rarity)
            cards (rarity (:cards set))]
        (->> (sample cards seed)
          (drop (-> (- (count cards) amount) (/ 2)))
          (take amount))))))

(defn booster
  ([set-code] (booster set-code (rand-seed)))
  ([set-code seed]
   (if-let [{:keys [cards] :as set} (get booster-sets set-code)]
     (let [booster-spec (remove #{"marketing"} (:booster set))]
       (mapcat (fn [rarity-section]
                 (let [rarity (first rarity-section)
                       amount (count rarity-section)]
                   (sample-cards-by-rarity set rarity amount seed)))
               (partition-by identity booster-spec))))))

(defn pool
  ([set-codes] (pool set-codes (rand-seed)))
  ([set-codes seed]
   (let [gnr8r (java.util.Random. seed)]
     (repeatedly 10 #(.nextLong gnr8r))
     (mapcat booster set-codes (repeatedly #(.nextLong gnr8r))))))

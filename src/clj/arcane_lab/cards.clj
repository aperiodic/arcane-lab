(ns arcane-lab.cards
  (:require [arcane-lab.utils :refer [rand-seed sample words->key]]
            [bigml.sampling.simple]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [cheshire.core :as json]))

;;
;; Definitions
;;

(def rare-slot #{:rare :mythic-rare})
(def foil-slot #{:foil-common :foil-uncommon :foil-rare :foil-mythic-rare})

(def variable-slot-weights
  {rare-slot {:rare 7 :mythic-rare 1}
   foil-slot {:common 10, :uncommon 3, :rare 1, :mythic-rare 0.125}})

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

(def ignored-sets
  #{:EVG :DD2 :DD3 :DD3_DVD :DD3_EVG :DD3_GVL :DD3_JVC :DDC :DDD :DDE :DDF :DDG
    :DDH :DDI :DDJ :DDK :DDL :DDM :DDN :DDO :TPR :MED :ME2 :ME3 :ME4 :VMA :MD1
    :H09 :PD2 :PD3 :DKM :DPA :ARC :DRB :V09 :V10 :V11 :V12 :V13 :V14 :V15})

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
    (into {} (for [[code set] raw-sets
                   :when (and (not= (:type set) "promo")
                              (not (contains? ignored-sets code)))]
               [code (update-in set [:cards] (partial map process-card))]))))

(def booster-sets
  (into {} (for [[code set] all-sets
                 :when (contains? set :booster)]
             [code (process-booster-set set)])))

;;
;; Card Search
;;

(defn printings-in-set
  [nombre magic-set]
  (let [canonical-nombre (str/lower-case nombre)
        cards (:cards magic-set)
        cardseq (if (map? cards)
                  (reduce concat (vals cards))
                  cards)]
    (->> cardseq
      (filter #(= (-> % :name str/lower-case) canonical-nombre))
      (map #(assoc % :set (:code magic-set))))))

(defn printing-in-set
  [nombre magic-set]
  (first (printings-in-set nombre magic-set)))

(defn printings
  [nombre]
  (case nombre
    "Plains" (->> (printings-in-set "Plains" (:ZEN booster-sets))
               (filter #(= (:multiverseid %) 195179)))
    "Island" (->> (printings-in-set "Island" (:ZEN booster-sets))
               (filter #(= (:multiverseid %) 195170)))
    "Swamp" (->> (printings-in-set "Swamp" (:ZEN booster-sets))
              (filter #(= (:multiverseid %) 201977)))
    "Mountain" (->> (printings-in-set "Mountain" (:ZEN booster-sets))
                 (filter #(= (:multiverseid %) 201970)))
    "Forest" (->> (printings-in-set "Forest" (:ZEN booster-sets))
               (filter #(= (:multiverseid %) 195183)))
    (->> (map (partial printing-in-set nombre) (vals all-sets))
      (keep identity))))

(defn booster-printings
  [nombre]
  (->> (map (partial printing-in-set nombre) (vals booster-sets))
    (keep identity)))

(defn find-extremum-by
  [relation attribute maps]
  (reduce (fn [winner candidate]
            (let [max-val (if winner (get winner attribute))
                  cand-val (if candidate (get candidate attribute))]
              (cond
                (nil? max-val) candidate
                (nil? cand-val) winner
                (relation cand-val max-val) candidate
                :else winner)))
          nil
          maps))

(def find-max-by (partial find-extremum-by >))
(def find-min-by (partial find-extremum-by <))

(defn newest-printing
  [printings]
  (find-max-by :multiverseid printings))

(defn oldest-printing
  [printings]
  (find-min-by :multiverseid printings))

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

(defn sample-booster-slots
  [magic-set slot amount seed]
  (let [magic-set-code (keyword (:code magic-set))]
    (if (= slot :land)
      (if-let [sampler (special-land-sampler magic-set-code)]
        [(sampler seed)]
        (->> (sample basic-land-cards seed) (take amount)))
      ;; else (not a land slot, should be same for all sets)
      (let [rarity (if-not (coll? slot)
                     slot
                     (let [weights (variable-slot-weights (set slot))]
                       (-> (bigml.sampling.simple/sample
                             (keys weights)
                             :seed seed :replace true :weigh weights :generator :twister)
                         (nth 10))))
            cards (get (:cards magic-set) rarity)]
        (->> (sample cards seed)
          (drop (-> (- (count cards) amount) (/ 2)))
          (take amount))))))

(defn booster
  ([set-code] (booster set-code (rand-seed)))
  ([set-code seed]
   (if-let [{:keys [cards] :as set} (get booster-sets set-code)]
     (let [booster-spec (remove #{"marketing"} (:booster set))]
       (mapcat (fn [rarity-section]
                 (let [slot (first rarity-section)
                       amount (count rarity-section)]
                   (sample-booster-slots set slot amount seed)))
               (partition-by identity booster-spec))))))

(defn pool
  ([set-codes] (pool set-codes (rand-seed)))
  ([set-codes seed]
   (let [gnr8r (java.util.Random. seed)]
     (repeatedly 10 #(.nextLong gnr8r))
     (mapcat booster set-codes (repeatedly #(.nextLong gnr8r))))))

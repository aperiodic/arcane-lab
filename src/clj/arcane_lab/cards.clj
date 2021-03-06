(ns arcane-lab.cards
  (:require [arcane-lab.color :as color]
            [arcane-lab.data :as data]
            [arcane-lab.sets :as sets]
            [arcane-lab.utils :refer [fractional? integral? rand-seed sample
                                      seeded-rng str->long words->key]]
            [bigml.sampling.simple]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [dlp.useful.fn :as fn]
            [dlp.useful.map :refer [merge-values]]))

;;
;; Definitions
;;

(def rarities [:common :uncommon :rare :mythic-rare])
(def sheets [:commons :uncommons :rares :dfcs :lands :timeshifted])

(def rare-slot #{:rare :mythic-rare})
(def foil-slot #{:foil-common :foil-uncommon :foil-rare :foil-mythic-rare})
(def checklist-slot #{:land :checklist})
(def soi-maybe-dfc-slot #{:double-faced :common})

(def variable-slot-weights
  {foil-slot {:commons 80, :uncommons 24, :rares 9}
   soi-maybe-dfc-slot {:commons 7 :dfcs 1}})

(def basic-names #{"Plains" "Island" "Swamp" "Mountain" "Forest"})

(defn basic-land?
  [card]
  (contains? basic-names (:name card)))

(defn nonbasic-rarity
  "Return the rarity of any non-basic-land card; for the basics, return
  :basic-land."
  [card]
  (if (basic-land? card)
    :basic-land
    (:rarity card)))

(defn dfc?
  [card]
  (= (:layout card) "double-faced"))

(defn split-card?
  [card]
  (or (= (:layout card) "split")
      (= (:layout card) "aftermath")))

(defn composite-card?
  [card]
  (or (split-card? card) (dfc? card)))

(defn second-part?
  "Return true if this card is the second half of a card, meaning the other part
  of a split card or the back of a double-faced card (flip cards not supported
  yet)."
  [card]
  (and (number? (:number card))
       (fractional? (:number card))))

(defn melded-card?
  [card]
  (boolean (:melded card)))

(defn meld-card?
  [card]
  (or (melded-card? card)
      (-> (re-find #"\b(M|m)elds?\b" (or (:text card) ""))
        boolean)))

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

(def amonkhet-tap-land-names #{"Cinder Barrens"
                               "Forsaken Sanctuary"
                               "Foul Orchard"
                               "Highland Lake"
                               "Meandering River"
                               "Stone Quarry"
                               "Submerged Boneyard"
                               "Timber Gorge"
                               "Tranquil Expanse"
                               "Woodland Stream"})

(def guildgate-names #{"Azorius Guildgate"
                       "Boros Guildgate"
                       "Dimir Guildgate"
                       "Golgari Guildgate"
                       "Gruul Guildgate"
                       "Izzet Guildgate"
                       "Orzhov Guildgate"
                       "Rakdos Guildgate"
                       "Selesnya Guildgate"
                       "Simic Guildgate"})

;;
;; Card & Set Processing
;;

(def ignored-sets
  #{:EVG :DD2 :DD3 :DD3_DVD :DD3_EVG :DD3_GVL :DD3_JVC :DDC :DDD :DDE :DDF :DDG
    :DDH :DDI :DDJ :DDK :DDL :DDM :DDN :DDO :TPR :MED :ME2 :ME3 :ME4 :VMA :MD1
    :H09 :PD2 :PD3 :DKM :DPA :ARC :DRB :V09 :V10 :V11 :V12 :V13 :V14 :V15
    ;; TODO: fix Conspiracy 2 by adding support for draft matters booster slot
    :CN2})

(defn extraneous-card-predicate
  "This is for sets that need to have some cards removed that are technically in
  the set for some reason but do not appear in the set's booster packs. It's
  a mapping between a (keyword) set code and a predicate function that detects
  these extraneous cards."
  [the-set]
  (let [no-multiverse-id? #(not (contains? % :multiverse-id))
        ;; Often sets have extra cards printed for the introductory Planeswalker
        ;; decks that are technically in the set but don't show up in boosters.
        ;; These cards have a collector number larger than the set's size.
        number-too-big? (fn [card]
                          (let [collector-number (-> (:number card) str->long (or 0))]
                            (> collector-number (:base-set-size the-set))))
        GRN-RNA-second-guild-gate? (fn [c] (.endsWith (:name c) " (b)"))]

    (case (-> (:code the-set) keyword)
      :8ED #(string? (:number %)) ;; Cards in these sets that were printed in
      :9ED #(string? (:number %)) ;; decks but not boosters have numbers like
                                  ;; "S1", which are left as strings by the
                                  ;; `parse-collector-number` function below.
      :EMN :melded
      (:GRN :RNA) (fn/any? no-multiverse-id?
                           number-too-big?
                           GRN-RNA-second-guild-gate?)

      ;default
      (fn/any? no-multiverse-id? number-too-big?))))

(defn trim-raw-card
  "Given a raw card map (i.e. loaded directly from cards-by-set.json), return
  a 'trimmed' version without fields we don't care about (like 'foreignData')"
  [raw-card]
  (dissoc raw-card
          :foreignData
          :tcgplayerProductId
          :tcgplayerPurchaseUrl
          :rulings
          :prices
          :purchaseUrl))

(defn parse-collector-number
  "Parse a collector number to an integer or float. Most parsed collector
  numbers are integers, but the numbers of double-faced & split cards (which are
  'Xa' and 'Xb' for the front & back sides or the first & second split cards)
  get turned into X.0 and X.5. There's a special case for the Unhinged card
  'Who/What/When/Where/Why', which gets numbers 120.0, 120.5, 120.6, 120.7, and
  120.8 for each of the sub-cards in order."
  [number-string]
  (cond
    (nil? number-string)
    nil

    ;; if collector number doesn't even start with an integer, bail out
    (nil? (str->long number-string))
    number-string

    :else ;; collector number must at least start with an integer
    (if-let [side (re-find #"a|b|c|d|e" number-string)]
      (+ (str->long number-string)
         (case side
           "a" 0.0
           "b" 0.5
           "c" 0.6
           "d" 0.7
           "e" 0.8))
      ;; else (regular collector number, possibly preceded by '★')
      (if (re-find #"^★" number-string)
        (recur (subs number-string 1))
        (try (Integer/parseInt number-string)
          (catch java.lang.NumberFormatException _
            number-string))))))

(defn fix-split-color-id
  "Cards with a split layout sometimes do not have the correct color identity
  filled out, which should include both halves of the card."
  [card]
  (if-not (split-card? card)
    card
    (let [{:keys [other-part]} card
          ids (-> (concat (:color-identity card ) (:color-identity other-part))
                distinct
                vec)]
      (-> card
        (assoc :color-identity ids)
        (assoc-in [:other-part :color-identity] ids)))))

(def set-field-translations
  {:baseSetSize :base-set-size,
   :boosterV3 :booster,
   :releaseDate :release-date
   :totalSetSize :total-set-size})

(def card-field-translations
  {:borderColor :border-color
   :colorIdentity :color-identity
   :convertedManaCost :converted-mana-cost
   :flavorText :flavor-text
   :frameVersion :frame-version
   :hasFoil :has-foil
   :hasNonFoil :has-non-foil
   :manaCost :mana-cost
   :multiverseId :multiverse-id
   :originalText :original-text
   :originalType :original-type
   :scryfallId :scryfall-id})

(def camels->snakes card-field-translations)
(def snakes->camels (into {} (for [[c s] camels->snakes]
                               [s c])))

(defn summon-snakes
  "Transmute the camelCased field names to snake-style."
  [card]
  (set/rename-keys card camels->snakes))

(defn summon-camels
  "Transmute snake-style the field names to camelCased."
  [card]
  (set/rename-keys card snakes->camels))

(defn keywordize-and-categorize
  "Process a card by:
    1 - keywordizing colors & color identities
    2 - keywordizing with words->key
    3 - parsing collector number as an integer (if possible)
    4 - add the boolean predicate fields :dfc? and :composite?"
  [card]
  (-> card
    (update :border-color keyword)  ;; there's only a handful of unique values
    (update :frame-version keyword) ;; for these fields, so intern them as KWs
    (update :colors (partial mapv words->key))
    (update :color-identity (partial mapv color/abbrev->color))
    (update :rarity words->key)
    (update :number parse-collector-number)
    (assoc :dfc? (dfc? card))
    (assoc :composite? (composite-card? card))))

(defn front-side?
  [card]
  (if-not (:dfc? card)
    true
    (and (number? (:number card))
         (integral? (:number card)))))

(def shadows-block-booster
  (concat [(vec rare-slot)]
          (repeat 3 :uncommon)
          (repeat 8 :common)
          [(vec soi-maybe-dfc-slot) :double-faced :land]))

(def normal-booster
  (concat [(vec rare-slot)]
          (repeat 3 :uncommon)
          (repeat 10 :common)
          [:land]))

(def special-booster-set-processor
  "Post-processing for booster sets that require more than just removing
  extraneous cards. Currently, this means
    * FRF: removing the Khans refuges from Fate Reforged's commons, since they
      only ever show up in the land slot.
    * DKA, SOI, EMN: fixing the booster slot definitions.
  "
  {:FRF (fn [frf]
          ;; Fate Reforged contains the refuges also printed in Khans, but they
          ;; should only show up in the land slot, never in the comons.
          (update-in frf [:cards :common] (partial remove #(contains? refuge-names (:name %)))))
   :DKA (fn [dka]
          (assoc dka :booster (concat [(vec rare-slot)]
                                      (repeat 3 :uncommon)
                                      (repeat 9 :common)
                                      [:double-faced [:land :checklist]])))
   :SOI #(assoc % :booster shadows-block-booster)
   :EMN #(assoc % :booster shadows-block-booster)
   :KLD #(assoc % :booster normal-booster) ; mtgjson data has a draft matters slot for KLD (?!)
   :GRN (let [grn-guildgate-renamer (fn [c] (if (re-find #"Guildgate" (:name c))
                                              (update c :name str/replace #" \((a|b)\)" "")
                                              c))]
          #(update-in % [:cards :common] (partial map grn-guildgate-renamer)))})

(defn link-composite
  "Find the other part of a composite card in the set and return an updated
  version of composite-card that has the other part in the :other-part field. If
  the card is double-faced, the other part is also added in the :reverse field."
  [composite-card set]
  (if-not (:composite? composite-card)
    composite-card
    (let [{this-name :name, names :names} composite-card
          {[other-name] false} (group-by (partial = this-name) names)
          other-part (->> (:cards set)
                       (filter #(= (:name %) other-name))
                       first)]

      (cond-> (assoc composite-card
                     :other-part (dissoc other-part :other-part)
                     :names [this-name other-name])
        (:dfc? composite-card) (assoc :reverse (dissoc other-part :reverse))
        (split-card? composite-card) fix-split-color-id))))

(defn link-composites
  [set]
  (update set :cards (fn [cards] (map #(link-composite % set) cards))))

(def dfc-sets
  "Note that Origins should not be here because its DFCs do not get their own
  sheet and booster slots, so they work fine as regular old mythics."
  #{:ISD :DKA :SOI :EMN})

(defn move-dfcs
  "Move the dfcs in the given cards map of a set to their own sheet, removing
  them from the normal rarity fields."
  [set]
  (if-not (contains? dfc-sets (-> set :code keyword))
    set
    (let [cards (:cards set)]
      (assoc set :cards (let [dfcs (->> (vals cards)
                                     (apply concat)
                                     (filter :dfc?))]
                          (-> (into {} (for [[r r-cards] cards]
                                         [r (remove :dfc? r-cards)]))
                            (assoc :dfcs dfcs)))))))

(defn process-booster-set
  "Pre-process a set to:
    - link the two halves of split cards and DFCs
    - remove extraneous cards that are not printed in booster packs
    - group cards by rarity (with DFCs in their own rarity)
    - change string values in booster specs to keywords
    - remove the marketing slot from booster specs
    - perform any special processing particular to the set
  "
  [booster-set]
  (let [keywordize-string (fn [x]
                            (if (string? x)
                              (words->key x)
                              x))
        code (-> booster-set :code keyword)
        special-processor (special-booster-set-processor code identity)
        extraneous-card? (fn/any?
                           second-part?
                           (extraneous-card-predicate booster-set))]
    (-> booster-set
      (update :cards (partial remove extraneous-card?))
      (update :cards (partial group-by nonbasic-rarity))
      move-dfcs
      (update :booster (partial postwalk keywordize-string))
      (update :booster (partial remove #{:marketing :token}))
      special-processor)))

;;
;; Load & Process Sets
;;

(defn raw-cards-by-set
  []
  (-> (io/resource "cards-by-set.json")
    slurp
    (json/decode true)))

(def all-sets
  (let [raw-sets (-> (raw-cards-by-set)
                   ;; EMN comes from its own data file because the MtGJSON
                   ;; version has bad data from Gatherer that they refuse to fix
                   (assoc :EMN (data/eldritch-moon)))
        process-card (comp keywordize-and-categorize summon-snakes trim-raw-card)]
    (into {} (for [[code mtg-set] raw-sets
                   :let [mtg-set (set/rename-keys mtg-set set-field-translations)]
                   :when (and (not= (:type mtg-set) "promo")
                              (not (contains? ignored-sets code)))]
               [code
                (-> mtg-set
                  (update :cards (partial map process-card))
                  link-composites
                  (cond->
                    (:booster mtg-set)
                    (assoc :sealed-format (get sets/sealed-formats
                                            code
                                            (str "6" (name code))))))]))))

(def booster-sets
  (into {} (for [[code mtg-set] all-sets
                 :when (contains? mtg-set :booster)]
               [code (process-booster-set mtg-set)])))

;;
;; Set Predicates
;;

(defn set-code?
  [set-code]
  (contains? all-sets set-code))

(defn booster-set-code?
  [set-code]
  (contains? booster-sets set-code))

(defn lands-in-boosters?
  [set-code]
  (let [booster-spec (-> (get-in booster-sets [set-code :booster]) flatten set)]
    (contains? booster-spec :land)))

(defn dfcs-in-boosters?
  [set-code]
  (let [booster-spec (-> (get-in booster-sets [set-code :booster]) flatten set)]
    (contains? booster-spec :double-faced)))

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
               (filter #(= (:multiverse-id %) 195179)))
    "Island" (->> (printings-in-set "Island" (:ZEN booster-sets))
               (filter #(= (:multiverse-id %) 195170)))
    "Swamp" (->> (printings-in-set "Swamp" (:ZEN booster-sets))
              (filter #(= (:multiverse-id %) 201977)))
    "Mountain" (->> (printings-in-set "Mountain" (:ZEN booster-sets))
                 (filter #(= (:multiverse-id %) 201970)))
    "Forest" (->> (printings-in-set "Forest" (:ZEN booster-sets))
               (filter #(= (:multiverse-id %) 195183)))
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
  (find-max-by :multiverse-id printings))

(defn oldest-printing
  [printings]
  (find-min-by :multiverse-id printings))

;;
;; Print Runs
;;

(defn- cycles
  [n coll]
  (take (* n (count coll)) (cycle coll)))

(defn- stochastic-slot->sheet
  [slot seed]
  (let [slot-space (mapcat (fn [[kind weight]] (repeat weight kind))
                           (get variable-slot-weights soi-maybe-dfc-slot))]
    (first (sample slot-space seed))))

(defn slot->sheet
  "Throws an error if given a slot it doesn't know how to handle.

  Currently unhandled slots (non-exhaustive list):
    - TSP :timeshifted-purple
    - PLC :timeshifted-common, :timeshifted-uncommon, :timeshifted-rare
    - FUT :timeshifted-common, :timeshifted-uncommon, :timeshifted-rare
    - MMA/MM2 #{:foil-mythic-rare :foil-rare :foil-uncommon :foil-common}"
  [slot seed]
  (let [slot (if (coll? slot)
               (set slot)
               slot)
        deterministic {rare-slot :rares
                       :rare :rares
                       :uncommon :uncommons
                       :common :commons
                       :land :lands
                       checklist-slot :lands
                       :double-faced :dfcs}
        stochastic #{soi-maybe-dfc-slot}]
    (cond
      (contains? deterministic slot) (deterministic slot)
      (contains? stochastic slot) (stochastic-slot->sheet slot seed)
      :else (throw (IllegalArgumentException.
                     (str "Don't know how to handle booster slot " slot))))))

(defn rare-sheet
  "Ordered 'sheet' of all the rares in the set with the given code, with the
  proper proportion of normal rares & mythics if the set has both."
  [set-code]
  (let [rares (get-in booster-sets [set-code :cards :rare])
        mythics (get-in booster-sets [set-code :cards :mythic-rare])]
    (if (empty? mythics)
      rares
      (concat mythics (cycles 2 rares)))))

(defn uncommons-sheet
  [set-code]
  (get-in booster-sets [set-code :cards :uncommon]))

(defn commons-sheet
  [set-code]
  (case set-code
    :M19 (remove #(contains? amonkhet-tap-land-names (:name %))
                 (get-in booster-sets [set-code :cards :common]))
    (get-in booster-sets [set-code :cards :common])))

(defn dfcs-sheet
  [set-code]
  (if-not (dfcs-in-boosters? set-code)
    ()
    (let [dfc-fronts (->> (get-in booster-sets [set-code :cards :dfcs])
                       (filter front-side?))
          {:keys [common uncommon rare mythic-rare]} (group-by :rarity dfc-fronts)]
      (concat mythic-rare
              (cycles 2 rare)
              (cycles 6 uncommon)
              (cycles 11 common)))))

(defn lands-sheet
  [set-code]
  (let [default-basics (get-in booster-sets [:ODY :cards :basic-land])]
    (case set-code
      :FRF (let [ktk-fetches (filter #(contains? ally-fetch-names (:name %))
                                     (get-in booster-sets [:KTK :cards :rare]))
                 frf-refuges (filter #(contains? refuge-names (:name %))
                                     (get-in all-sets [:FRF :cards]))]
             ;; oops, this one should have been shuffled
             (concat (take 105 (cycle frf-refuges))
                     (take 5 (cycle ktk-fetches))))

      :OGW default-basics ; wastes are basic but not on the land sheet

      :M19 (let [tap-lands (filter #(contains? amonkhet-tap-land-names (:name %))
                                   (get-in booster-sets [:M19 :cards :common]))
                 basics (get-in booster-sets [:M19 :cards :basic-land])]
             ;; normally land sheet is not shuffled but this one should be
             (shuffle (concat (take 60 (cycle basics))
                              (take 40 (cycle tap-lands)))))

      :GRN (let [guildgates (filter #(contains? guildgate-names (:name %))
                                    (get-in booster-sets [:GRN :cards :common]))]
             ;; reduces variance in copies of any one gate in a sealed pool
             (shuffle (take 15 guildgates)))

      ;; normally just use basics printed in the set
      (or (get-in booster-sets [set-code :cards :basic-land])
          default-basics))))

(defn print-run
  ([set-code] (print-run set-code (rand-seed)))
  ([set-code seed]
   (if-not (contains? booster-sets set-code)
     (throw (IllegalArgumentException. (str set-code " is not a valid set code")))
     (merge {:set-code set-code
             :seed seed
             :rares (sample (rare-sheet set-code) seed)
             :uncommons (sample (uncommons-sheet set-code) seed)
             :commons (sample (commons-sheet set-code) seed)
             :boosters []}
            (if (dfcs-in-boosters? set-code)
              {:dfcs (sample (dfcs-sheet set-code) seed)})
            (if (lands-in-boosters? set-code)
              {:lands (lands-sheet set-code)})))))

(defn rares-empty?
  [print-run]
  (< (count (:rares print-run)) 1))

(defn uncommons-empty?
  [print-run]
  (< (count (:uncommons print-run)) 3))

(defn commons-empty?
  [print-run]
  (let [{:keys [commons set-code]} print-run]
    (if (lands-in-boosters? set-code)
      (< (count commons) 10)
      (< (count commons) 11))))

(defn dfcs-empty?
  "Ok, so this one is a little weird, because we never want to add DFCs if there
  are really none there. Consequently, if the set has no DFCs, the DFCs are
  never empty."
  [print-run]
  (if-not (dfcs-in-boosters? (:set-code print-run))
    false
    (< (count (:dfcs print-run)) 2)))

(defn lands-empty?
  "Ok, so this one is a little weird, because we never want to add lands if
  there are really none there. Consequently, if the set has no lands, the lands
  are never empty."
  [print-run]
    (if-not (lands-in-boosters? (:set-code print-run))
      false
      (< (count (:lands print-run)) 1)))

(defn print-run-empty?
  "Returns true if you can't build a booster pack using the given print run."
  [print-run]
  (or (rares-empty? print-run)
      (uncommons-empty? print-run)
      (commons-empty? print-run)
      (lands-empty? print-run)
      (dfcs-empty? print-run)))

(defn replenish-print-run
  [print-run]
  (let [{:keys [set-code seed]} print-run
        seed' (.nextLong (seeded-rng seed))
        shuffle #(sample % seed')]
    (cond-> (assoc print-run :seed seed')
      (rares-empty? print-run) (update :rares concat (shuffle (rare-sheet set-code)))
      (uncommons-empty? print-run) (update :uncommons concat (shuffle (uncommons-sheet set-code)))
      (commons-empty? print-run) (update :commons concat (shuffle (commons-sheet set-code)))
      (lands-empty? print-run) (update :lands concat (lands-sheet set-code))
      (dfcs-empty? print-run) (update :dfcs concat (dfcs-sheet set-code)))))

;;
;; Booster Sampling
;;

(defn cards->booster
  [cards]
  (-> (group-by nonbasic-rarity cards)
    (merge-values :rare :mythic-rare)))

(defn print-booster
  "Takes a print run and an optional booster seed, and returns an updated
  version of the print run with an additional booster pack in the :boosters
  field, and possibly with new sheets in the card fields if they were necessary
  in order to print the pack.
  If the booster seed is not provided, then the print run's seed is used. This
  is fine if you're just printing one booster, but if you're printing multiple
  boosters you'll probably want to supply different seeds for each booster so
  they'll have the chance to make different decisions for any stochastic slots
  they might have (such as SOI's maybe-DFC-probably-common slot). Naturally, if
  you're making something like a sealed pool where the contents should be
  reproducible from a single seed, you'll want to derive the booster seeds from
  that seed."
  ([print-run] (print-booster print-run (:seed print-run)))
  ([print-run booster-seed]
   (if (print-run-empty? print-run)
     (recur (replenish-print-run print-run) booster-seed)
     (let [{:keys [seed set-code boosters]} print-run
           {:keys [booster] :as the-set} (booster-sets set-code)
           booster-sheets (map #(slot->sheet % seed) booster)
           sheet->qty (frequencies booster-sheets)
           !booster (atom [])
           sheets' (into {} (for [[kind sheet] (select-keys print-run sheets)]
                              (let [qty (sheet->qty kind 0)]
                                (swap! !booster concat (take qty sheet))
                                [kind (drop qty sheet)])))]
       (-> (merge print-run sheets')
         (update :boosters concat [(cards->booster @!booster)]))))))

(defn booster
  ([set-code] (booster set-code (rand-seed)))
  ([set-code seed]
   (-> (print-run set-code seed)
     print-booster
     :boosters
     first)))

(defn print-boosters
  "Add the given quantity of boosters to the print run's :boosters field,
  returning the updated run. The boosters will have seeds derived from the set
  seed so they'll have the chance to make different choices for stochastic
  booster slots (such as SOI's maybe-DFC-probably-common slot)."
  [print-run quantity]
  (let [rng (seeded-rng (:seed print-run))]
    (reduce (fn [print-run booster-seed]
              (print-booster print-run booster-seed))
            print-run
            (repeatedly quantity #(.nextLong rng)))))

(def print-run-switch-threshold (/ 1 36.0))

(defn booster->cards
  "Flatten a booster to a sequence of cards."
  [booster]
  (let [{:keys [common uncommon rare basic-land]} booster
        stray-cards (->> (dissoc booster :common :uncommon :rare :basic-land)
                      vals
                      (apply concat))]
    (concat common uncommon rare basic-land stray-cards)))

(defn pool
  ([set-codes] (pool set-codes (rand-seed)))
  ([set-codes seed]
   (let [pack-count (frequencies set-codes)
         total-packs (reduce + (vals pack-count))
         rng (seeded-rng seed)]
     (:boosters
       (reduce (fn [{:keys [current-print-run switched-runs? boosters]} [set-code booster-seed]]
                (let [print-run' (-> (if (= (:set-code current-print-run) set-code)
                                       current-print-run
                                       (print-run set-code (.nextLong rng)))
                                   (print-booster booster-seed))
                      switch? (and (< (.nextDouble rng) print-run-switch-threshold)
                                   (not switched-runs?))]

                  {:current-print-run (if switch?
                                        (print-run set-code (.nextLong rng))
                                        print-run')
                   :switched-runs? (if (= set-code (:set-code current-print-run))
                                     (or switched-runs? switch?)
                                     false)
                   :boosters (concat boosters [(last (:boosters print-run'))])}))

              {:current-print-run (print-run (first set-codes) (.nextLong rng))
               :switched-runs? false
               :boosters []}

              (map vector set-codes (repeatedly #(.nextLong rng))))))))

(defn pool-by-sheets
  ([set-codes] (pool-by-sheets set-codes (rand-seed)))
  ([set-codes seed]
   (apply merge-with concat (pool set-codes seed))))

(ns scratch
  (:require [arcane-lab.cards :as cards]
            [arcane-lab.transform :as xform]
            [bigml.sampling.simple :as simple]
            [clojure.java.io :as io]
            [clojure.string :as str]
            :reload))

(def sets-chronologically
  (->> (sort-by (comp :releaseDate second)
                (filter (comp :booster second) (seq cards/all-sets)))
    reverse))

(def codes-chronologically (mapv first sets-chronologically))

(defn find-card-in-set
  [cardname card-set]
  (-> (filter #(= (:name %) cardname) (:cards card-set))
    first))

(defn card-printings
  [cardname]
  (let [!printings (atom [])]
    (doseq [[code set] sets-chronologically
            :when (not= code :VMA)]
      (when-let [{:keys [rarity]} (find-card-in-set cardname set)]
        (swap! !printings conj [code rarity])))
    @!printings))

(comment
  (take 10 codes-chronologically)
  (card-printings "Thoughtseize")

  (let [cube-cards (for [line (line-seq (io/reader "scratch/devotion_cube_to_acquire.txt"))
                         :let [trimmed (str/trim line)]
                         :when (not (empty? trimmed))]
                     trimmed)
        with-printings (for [card cube-cards]
                         [card (card-printings card)])
        sorted-by-last-printing (sort-by (fn [x] (-> x second first first (#(.indexOf codes-chronologically %))))
                                         with-printings)]

    (with-open [out-file (io/writer "scratch/devotion_cube_to_acquire_sorted_by_last_printing.txt")]
      (doseq [[card printings] sorted-by-last-printing]
        (.write out-file (str card " " (str/join " " printings) "\n"))))))

;; exploration of booster pack statistical properties
(comment
  (time (dotimes [_ 1e3]
          (cards/booster :KTK)
          ))

  (def freqs
    (let [boosters (repeatedly 2e6 #(map :name (cards/booster :KTK)))
          names (apply concat boosters)]
      (frequencies names)))

  (defn max-diff
    [k->i]
    (- (apply max (vals k->i))
       (apply min (vals k->i))))

  (defn max-percentage-diff
    [k->i]
    (-> (/ (max-diff k->i) (apply max (vals k->i)))
      (* 100) double))

  (def rarities [:mythic-rare :rare :uncommon :common])

  (let [ktk-cards (get-in cards/booster-sets [:KTK :cards])
        freqs-for-rarity (fn [r]
                           (->> (get ktk-cards r)
                             (map :name)
                             (select-keys freqs)))
        [mythic-freqs rare-freqs uncommon-freqs common-freqs] (for [r rarities]
                                                                (freqs-for-rarity r))
        ]

    (max-percentage-diff common-freqs)

    )

  (let [s (take 1000 (simple/sample #{:a :b} :replace true :weigh {:a 1 :b 2}))]
    (-> (/ (count (filter (partial = :a) s))
           (count (filter (partial = :b) s)))
      double)
    )
  )

;; rewriting MM2 cards to their earlier printings
(comment

  (->> (cards/printings "Tarmogoyf")
    (map :set)
    )

  (let [mm2 (:MM2 cards/booster-sets)
        printings (cards/printings "Lorescale Coatl")
        mm2-printing (cards/printing-in-set "Scion of the Wild" mm2)]
    (map (juxt :multiverseid :set) printings)
    )

  (let [mm2 (:MM2 cards/all-sets)
        find-earlier-printing (fn [{:keys [name] :as card}]
                                (let [mid' (->> (cards/printings name)
                                             (filter :multiverseid)
                                             (remove #(= (:set %) "MM2"))
                                             (remove #(= (:set %) "TPR"))
                                             (remove #(= (:set %) "DDO"))
                                             first
                                             :multiverseid)]
                                  (assoc card :multiverseid mid')))
        mm2' (update-in mm2 [:cards] (partial map find-earlier-printing))
        sets' (assoc cards/all-sets :MM2 mm2')
        ]
    (spit "resources/cards-by-set.json" (xform/serialize-sets sets'))
    )
  )

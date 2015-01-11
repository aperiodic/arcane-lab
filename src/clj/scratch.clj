(ns scratch
  (:require [arcane-lab.cards :as cards]
            [bigml.sampling.simple :as simple] :reload))

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

(ns arcane-lab.card)

(def rarities [:common :uncommon :rare :basic-land])

(defn rare?
  [card]
  (contains? #{:rare :mythic-rare} (:rarity card)))

(defn uncommon?
  [card]
  (= (:rarity card) :uncommon))

(defn common?
  [card]
  (= (:rarity card) :common))

(defn basic-land?
  [card]
  (let [basic-land-names #{"Plains" "Island" "Swamp" "Mountain" "Forest"}]
    (contains? basic-land-names (:name card))))

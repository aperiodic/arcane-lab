(ns arcane-lab.ui
  "This namespace is for UI-related logic in pure clojure, e.g. going from plain
  card maps to other maps representing piles of cards. Taking the pile maps
  and rendering them to something the user can see happens elsewhere"
  (:require [arcane-lab.color :as color]
            [arcane-lab.constants :as c]
            [arcane-lab.piles :as piles]
            [arcane-lab.useful :refer [rand-uuid]]
            [dlp.useful.map :refer [merge-values]]))

;;
;; UI Card Representation
;;

(defn card-img-src
  [multiverseid]
  (str "/img/" multiverseid))

(defn add-img-src
  [card]
  (assoc card :img-src (card-img-src (:multiverseid card))))

(defn api-card->ui-card
  [api-card]
  (cond-> api-card
    :always add-img-src
    :always (->
              (assoc :id (rand-uuid), :selected? false)
              (update :x (fnil identity c/half-gutter))
              (update :y (fnil identity c/half-gutter)))
    (:reverse api-card) (update :reverse add-img-src)))

(defn ui-pile
  "Transform all the cards in the pile to be full UI cards by augmenting
  them using `api-card->ui-card`"
  [pile]
  (update pile :cards (partial map api-card->ui-card)))

(defn ui-pool
  "Transform all the cards in the pool to be full UI cards by augmenting
  them using `api-card->ui-card`"
  [pool]
  (into {} (for [[sheet cards] pool]
             [sheet (map api-card->ui-card cards)])))

;;
;; Initial UI State Calculation
;;

(defn card->sort-color
  [card]
  (if (:dfc? card)
    (color/id->category (:colors card))
    (color/id->category (:color-identity card))))

(defn pool-categories
  [cards-by-sheet]
  (let [non-rare-non-land (-> cards-by-sheet
                            (dissoc :rare :basic-land)
                            vals
                            (->> (apply concat)))
        others-by-color (->> non-rare-non-land
                             (group-by card->sort-color))]
    (assoc others-by-color
           :rares (sort-by color/wubrggc-order (:rare cards-by-sheet)))))

(defn nth-pile
  [y n cards]
  (piles/make-pile cards (piles/x-of-column-indexed n) y))

(defn rare-pile
  [n rare]
  (nth-pile c/first-row-y n [rare]))

(def second-row-y (+ c/first-row-y c/card-height c/gutter))

(defn pool->piles
  [cards-by-sheet]
  (let [{rares :rares, :as all-cats} (-> (pool-categories cards-by-sheet) ui-pool)
        others (dissoc all-cats :rares)
        categories-with-cards (remove #(empty? (get others %)) color/categories)
        other-pile (fn [i category]
                     (nth-pile second-row-y i (->> (get others category)
                                                (sort-by :name))))]
    (->> (concat (map-indexed rare-pile rares)
                 (map-indexed other-pile categories-with-cards))
      (map ui-pile))))

(defn deck->piles
  [deck-cards]
  (->> deck-cards
    (map api-card->ui-card)
    (sort-by key (group-by :cmc deck-cards))
    (map-indexed (fn [i [_ cmc-cards]]
                   (piles/make-pile (sort-by :name cmc-cards)
                                    (piles/x-of-column-indexed i) c/half-gutter)))))

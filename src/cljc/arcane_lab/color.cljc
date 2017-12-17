(ns arcane-lab.color)

(def categories [:white :blue :black :red :green :gold :colorless])

(defn id->category
  [color-identity]
  (case (count color-identity)
    0 :colorless
    1 (first color-identity)
    :gold))

(def color->index
  {:white 0
   :blue 1
   :black 2
   :red 3
   :green 4})

(defn wubrggc-order
  "Key function to use with sort-by to sort cards in WUBRGGC order, meaning
  single-colored cards in WUBRG order, followed by gold cards and then colorless
  cards."
  [card]
  (let [{:keys [color-identity]} card]
    (case (count color-identity)
      0 6
      1 (color->index (first color-identity) 0)
      ; if two or more colors, it's a gold card, so index 5
      5)))

(defn abbrev->color
  "Turn an abbreviated color string (the kind found in costs, e.g. 'G' or 'U')
  into a color keyword."
  [abbrev]
  (case abbrev
    "W" :white
    "U" :blue
    "B" :black
    "R" :red
    "G" :green))

(defn cost->category
  "Return the color type indicated by a mana cost. If the cost contains only one
  color, return that color as a keyword. If it contains multiple colors, then
  return `:gold`. If the cost is coloress, return nil."
  [mana-cost]
  (if mana-cost
    (let [w-cost? (re-find #"W" mana-cost)
          u-cost? (re-find #"U" mana-cost)
          b-cost? (re-find #"B" mana-cost)
          r-cost? (re-find #"R" mana-cost)
          g-cost? (re-find #"G" mana-cost)
          gold-cost? (or (and w-cost? u-cost?)
                         (and u-cost? b-cost?)
                         (and b-cost? r-cost?)
                         (and r-cost? g-cost?)
                         (and g-cost? w-cost?)
                         (and w-cost? b-cost?)
                         (and u-cost? r-cost?)
                         (and b-cost? g-cost?)
                         (and r-cost? w-cost?)
                         (and g-cost? u-cost?))]
      (cond
        gold-cost? :gold
        w-cost? :white
        u-cost? :blue
        b-cost? :black
        r-cost? :red
        g-cost? :green
        :else :colorless))))

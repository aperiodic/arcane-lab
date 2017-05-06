(ns arcane-lab.color)

(def color-order [:white :blue :black :red :green :gold :colorless])

(def color->index
  {:white 0
   :blue 1
   :black 2
   :red 3
   :green 4})

(defn wubrggc-sort
  "Sort cards by WUBRG, with gold cards followed by colorless ones at the end."
  [card]
  (let [{:keys [colors]} card
        color-count (count colors)]
    (cond
      (zero? color-count) 6
      (> (count colors) 1) 5
      :otherwise (color->index (first colors) 0))))

(defn abbrev->color
  "Turn an abbreviated color string (the kind found in costs, e.g. 'G' or 'U') into a color keyword."
  [abbrev]
  (case abbrev
    "W" :white
    "U" :blue
    "B" :black
    "R" :red
    "G" :green))

(defn cost->colortype
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
        g-cost? :green))))

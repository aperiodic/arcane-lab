(ns arcane-lab.piles
  (:require [arcane-lab.constants :as c]
            [arcane-lab.geom :refer [within?]]))

(defn pile-height
  [{:keys [cards height] :as pile}]
  (cond
    (not pile) 0
    height height
    :else (let [covered (dec (count cards))] ;; every card but last is covered
            (+ c/card-height (* c/pile-stride covered)))))

(defn pile-card-count
  [pile]
  (if-not pile
    0
    (count (:cards pile))))

(defn within-pile?
  [pile x y]
  (let [{l :x, t :y} pile
        r (+ l c/card-width)
        b (+ t (pile-height pile))]
    (within? l r t b x y)))

(defn x-of-column-indexed
  [n]
  (+ (* n c/pile-spacing) c/half-gutter))

(defn make-pile
  ([cards] (make-pile cards
                      (-> (map :x cards) sort first)
                      (-> (map :y cards) sort first)))
  ([cards x y]
   (let [reposition (fn [i card]
                      (assoc card :x x, :y (+ y (* i c/pile-stride))))
         repositioned (map-indexed reposition cards)]
     {:cards (vec repositioned), :x x, :y y
      :height (pile-height {:cards cards})})))

(defn make-drag-pile
  [cards drag-x drag-y]
  (-> (make-pile cards drag-x drag-y)
    (assoc :dfcs? (boolean (some :dfc cards)))))

(defn pile-selected?
  [edges pile]
  (and (boolean edges)
       (let [[l r t b] edges
             {x :x, y :y} pile
             height (pile-height pile)]
         (within? (- l c/card-width) r (- t height) b x y))))

(defn row-spacings
  "Given the row y locations in an ascending sequence, return a sequence of the
  spacing between each row. Note that the returned sequence will have one less
  element than the input sequence."
  [row-ys]
  (if (= count row-ys 1)
    ()
    (->> (reductions #(- %2 %1) row-ys)
      (drop 1)
      (map-indexed (fn [i x] (if (zero? i) (+ x c/half-gutter) x)))
      (mapv #(- % c/half-gutter)))))

(defn row-height
  [row]
  (+ c/gutter (apply max (map pile-height (vals row)))))

(defn row-i-height
  [piles i]
  (row-height (-> (seq piles)
                (nth i)
                val)))

(defn row-card-height
  [row]
  (apply max (map pile-card-count (vals row))))

(defn max-pile-x
  "Given the piles map from the state, return the highest x-coordinate for a
  pile."
  [piles]
  (loop [rows (seq piles), x-max 0]
    (if-let [[_ row] (first rows)]
      (let [row-max (loop [xs (keys row)
                           row-max 0]
                      (if-let [x (first xs)]
                        (if (> x row-max)
                          (recur (next xs) x)
                          (recur (next xs) row-max))
                        row-max))]
        (if (> row-max x-max)
          (recur (next rows) row-max)
          (recur (next rows) x-max)))
      x-max)))

(defn row-for
  "Returns the last row whose y position is less than or equal to y's, or nil if
  no such row exists."
  [rows y]
  (loop [rows (seq rows), row-hit nil]
    (if-let [[row-y row] (first rows)]
      (if (<= row-y y)
        (recur (next rows) row)
        row-hit)
      row-hit)))

(defn last-row
  [rows]
  (-> rows
    last
    val))

(defn pile-for
  "Returns the last pile in row whose x position is less than or equal to x's,
  or nil no such pile exists in the row."
  [row x]
  (loop [piles (seq row), pile-hit nil]
    (if-let [[pile-x pile] (first piles)]
      (if (<= pile-x x)
        (recur (next piles) pile)
        pile-hit)
      pile-hit)))

(defn card-for
  "Returns the last card in pile whose y position is less than y, or nil"
  [pile y]
  (loop [cards (:cards pile), card-hit nil]
    (if-let [{card-y :y :as card} (first cards)]
      (if (<= card-y y)
        (recur (next cards) card)
        card-hit)
      card-hit)))

(defn pile-under
  [piles x y]
  (if-let [row (row-for piles y)]
    (if-let [pile (pile-for row x)]
      (if (within-pile? pile x y)
        pile))))

(defn card-under
  [piles x y]
  (if-let [row (row-for piles y)]
    (if-let [pile (pile-for row x)]
      (if (within-pile? pile x y)
        (card-for pile y)))))

(defn covered-card-selected-by
  "Given the edges of a selection, return a predicate that determines whether
  a covered card (i.e. not the top card) in a pile that overlaps the selection
  is selected by the selection."
  [selection-edges]
  (let [[_ _ t b] selection-edges]
    (fn [{y :y}]
      (and (<= y b)
           (>= (+ y c/pile-stride) t)))))

(defn top-card-selected-by
  "Given the edges of a selection, return a predicate that determines whether
  the top card in a pile that overlaps the selection is selected by the
  selection."
  [selection-edges]
  (let [[_ _ t b] selection-edges]
    (fn [{y :y}]
      (and (<= y b)
           (>= (+ y c/card-height) t)))))

(defn selection-edges
  "Given a selection, return the left & right x values and the top & bottom
  y values, in a vector in that order ([l r t b])."
  [selection]
  (let [{[x1 y1] :start, [x2 y2] :stop} selection
        x-asc? (<= x1 x2)
        y-asc? (<= y1 y2)]
    (vector (if x-asc? x1 x2)
            (if x-asc? x2 x1)
            (if y-asc? y1 y2)
            (if y-asc? y2 y1))))

(defn selection-check-boundaries
  [piles]
  (let [x-max (+ (max-pile-x piles) c/card-width 1)
        x-stride (+ c/card-width c/gutter)]
    {:vertical (interleave (range c/half-gutter x-max x-stride)
                           (range (+ c/half-gutter c/card-width)
                                  x-max, x-stride))
     :horizontal (concat (for [[row-y row] piles
                               i (range 0 (row-card-height row))]
                           (+ row-y (* i c/pile-stride)))
                         (distinct
                           (for [[row-y row] piles
                                 pile (vals row)]
                             (+ row-y (pile-height pile)))))}))

(defn pile-after-selection
  "Given a selection and a pile returns a new pile with the cards that the
  selection hits marked by setting their :selected? field to true."
  [selection pile]
  (if-not selection
    pile
    (let [[l r t b :as edges] (selection-edges selection)]
      (if-not (pile-selected? edges pile)
        (update-in pile [:cards] (partial mapv #(assoc % :selected? false)))
        (let [covered-selected? (covered-card-selected-by edges)
              top-selected? (top-card-selected-by edges)]
          (assoc pile :cards (loop [cards (:cards pile), out []]
                               (if-let [card (first cards)]
                                 (let [next-cards (next cards)
                                       top-card? (not next-cards)
                                       selected? (if top-card?
                                                   (top-selected? card)
                                                   (covered-selected? card))
                                       card' (assoc card :selected? selected?)]
                                   (if top-card?
                                     (conj out card')
                                     (recur next-cards (conj out card'))))
                                 ;; else (no more cards)
                                 out))))))))

(defn drop-zones
  [piles]
  (let [x-lim (+ (max-pile-x piles) (* 2 c/pile-spacing))
        row-ys (keys piles)
        last-y (+ (last row-ys) c/card-height c/gutter)
        next-row-ys (-> (drop 1 row-ys)
                      (concat (list last-y)))]
    {:vertical (range (+ c/card-width c/gutter)
                      x-lim, c/pile-spacing)
     :horizontal (for [[y0 y1] (->> (interleave (for [[y row] piles]
                                                  (+ y c/card-height))
                                                next-row-ys)
                                 (partition 2))]
                   (-> (+ y0 y1)
                     (/ 2)))}))

(defn move-row
  [piles y y']
  (let [row (get piles y)
        row' (reduce (fn [row {:keys [cards x]}]
                       (assoc row x (make-pile cards x y')))
                     row
                     (vals row))]
    (-> piles
      (dissoc y)
      (assoc y' row'))))

(defn rejigger-rows
  "After placing, the rows may be need to be moved up if the first row is now
  empty, they may be too far apart if cards were moved from the strictly tallest
  pile in a row, or they may be too close together if new cards were moved to
  the tallest pile in a row. Given the piles map, return a new piles map with
  all empty rows removed, the rest of the rows moved up accordingly, and the
  rows exactly as far apart as necessary (i.e., the max pile height in each row
  plus the gutter)."
  [piles]
  (let [old-ys (keys piles)
        piles' (if (= (first old-ys) c/half-gutter)
                 piles
                 (let [first-spacing (- (first old-ys) c/half-gutter)]
                   (reduce (fn [piles [y y']] (move-row piles y y'))
                           piles
                           (map vector
                                old-ys
                                (map #(- % first-spacing) old-ys)))))
        ys (keys piles')
        spacings (row-spacings ys)
        spacings' (for [row (butlast (vals piles'))]
                    (+ (apply max (map pile-height (vals row)))
                       c/gutter))
        ys' (reductions + c/half-gutter spacings')]
    (if (= spacings spacings')
      piles'
      (reduce (fn [piles' [y y']] (move-row piles' y y'))
              piles'
              (map vector ys ys')))))

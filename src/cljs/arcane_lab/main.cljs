(ns arcane-lab.main
  (:require [ajax.core :as async-http]
            [ajax.edn :refer [edn-response-format]]
            [arcane-lab.sets :as sets]
            [cljs-uuid-utils.core :refer [make-random-uuid]]
            [cljs.core.async :as async]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [goog.events :as events]
            [jamesmacaulay.zelkova.signal :as sig]
            [jamesmacaulay.zelkova.keyboard :as keys]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.time :as time]
            [jamesmacaulay.async-tools.core :as tools]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(def rand-uuid make-random-uuid)

(enable-console-print!)

;;
;; Constants
;;

(def card-width 222)
(def card-height 319)

(def em 18)

(def half-card-width (int (/ card-width 2)))
(def half-card-height (int (/ card-height 2)))

(def gutter (int (/ card-width 8)))
(def half-gutter (int (/ gutter 2)))
(def pile-stride (int (/ card-height 9.5)))
(def pile-spacing (+ card-width gutter))

(def mouse-y-offset 48)

(def u-key-code 85)
(def r-key-code 82)

;; not technically constant, but will be for my lifetime
(def ts-digits (-> (js/Date.) .getTime (/ 1000) int str count))

;;
;; Card Creation & Sorting
;;

(defn card-img-src
  [multiverseid]
  (str "/img/" multiverseid))

(defn add-img-src
  [card]
  (assoc card :img-src (card-img-src (:multiverseid card))))

(defn api-card->client-card
  [api-card]
  (let [{:keys [multiverseid]} api-card
        reverse-side (:reverse api-card)]
    (cond-> api-card
      identity add-img-src
      identity (assoc :id (rand-uuid)
                      :x half-gutter, :y half-gutter
                      :selected? false)
      reverse-side (update :reverse add-img-src))))

(defn card-at
  ([card [x y]] (card-at card x y))
  ([card x y] (assoc card :x x :y y)))

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

(defn basic-land?
  [card]
  (let [basic-land-names #{"Plains" "Island" "Swamp" "Mountain" "Forest"}]
    (contains? basic-land-names (:name card))))

(defn rare?
  [card]
  (contains? #{:rare :mythic-rare} (:rarity card)))

(defn cost->colortype
  "Return the color type found by looking at the mana cost only. If the cost
  contains only one color, return that color as a keyword. If it contains
  multiple colors, then return `:gold`. If the cost is coloress, return nil."
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

;;
;; Selection / Geometric Filtering
;;

(defn within?
  "Returns true if the point at (x,y) is within the box defined by left, right,
  top, and bottom."
  [left right top bottom x y]
  (and (not (> x right))  ; the point is not to the right of the box
       (not (< x left))   ; the point is not to the left of the box
       (not (> y bottom)) ; the point is not below the box
       (not (< y top))))  ; the point is not above the box

(defn selection-edges
  "Given a selection, return the left & right x values and the top & bottom
  y values, in a vector in that order ([l r t b])."
  [selection]
  (let [{[x1 y1] :start, [x2 y2] :stop} selection]
    (vec (concat (sort [x1 x2])
                 (sort [y1 y2])))))

;;
;; Piles
;;

(defn pile-height
  [{:keys [cards] :as pile}]
  (if-not pile
    0
    ;; by induction, each card besides the first is covered by the previous
    (let [covered (dec (count cards))]
      (+ card-height (* pile-stride covered)))))

(defn within-pile?
  [pile x y]
  (let [{l :x, t :y} pile
        r (+ l card-width)
        b (+ t (pile-height pile))]
    (within? l r t b x y)))

(defn x-of-column-indexed
  [n]
  (+ (* n pile-spacing) half-gutter))

(defn make-pile
  ([cards] (make-pile cards
                      (-> (map :x cards) sort first)
                      (-> (map :y cards) sort first)))
  ([cards x y]
   (let [reposition (fn [i card] (assoc card :x x, :y (+ y (* i pile-stride))))
         repositioned (map-indexed reposition cards)]
     {:cards repositioned, :x x, :y y})))

(defn state->cards
  "Given a state with :piles, return all the cards in all the piles."
  [state]
  (->> (vals (:piles state))
    (mapcat vals)
    (mapcat :cards)))

(defn add-pile
  [state pile]
  (let [{:keys [x y]} pile]
    (-> state
       (update-in [:piles y] (fnil identity (sorted-map)))
       (assoc-in [:piles y x] pile))))

(defn remove-pile
  [state x y]
  (let [row (get-in state [:piles y])]
    (if (and (contains? row x) (= (count row) 1))
      (update-in state [:piles] dissoc y)
      (update-in state [:piles y] dissoc x))))

(defn pile-selected?
  [selection pile]
  (and (boolean selection)
       (let [[l r t b] (selection-edges selection)
             {x :x, y :y} pile
             height (pile-height pile)]
         (within? (- l card-width) r (- t height) b x y))))

(defn row-spacings
  "Given the row y locations in an ascending sequence, return a sequence of the
  spacing between each row. Note that the returned sequence will have one less
  element than the input sequence."
  [row-ys]
  (if (= count row-ys 1)
    ()
    (->> (reductions #(- %2 %1) row-ys)
      (drop 1))))

(defn row-height
  [row]
  (+ gutter (apply max (map pile-height (vals row)))))

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

(defn pile-after-selection
  "Given a selection and a pile returns a new pile with the cards that the
  selection hits marked by setting their :selected? field to true."
  [selection pile]
  (cond
    (not selection)
    pile

    (not (pile-selected? selection pile))
    (update-in pile [:cards] (partial mapv #(assoc % :selected? false)))

    :otherwise
    (let [[l r t b] (selection-edges selection)
          {cards :cards} pile
          uncovered (last cards)
          select-uncovd? (let [{y :y} uncovered]
                           (and (not (> y b))
                                (not (< (+ y card-height) t))))
          covered (butlast cards)
          covd-selected? (fn [{y :y}]
                           (and (not (> y b))
                                (not (< (+ y pile-stride) t))))
          selected-covd-ids (->> (filter covd-selected? covered) (map :id))
          selected-ids (if select-uncovd?
                         (concat selected-covd-ids (list (:id uncovered)))
                         selected-covd-ids)
          cards' (loop [cs cards, slctd-ids selected-ids, cs' []]
                   (if-let [{:keys [id] :as c} (first cs)]
                     (let [selected? (= id (first slctd-ids))
                           c' (assoc c :selected? selected?)]
                       (if selected?
                         (recur (next cs) (next slctd-ids) (conj cs' c'))
                         (recur (next cs) slctd-ids (conj cs' c'))))
                     ;; else (no more cards)
                     cs'))]
      (assoc pile :cards cards'))))

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
        piles' (if (= (first old-ys) half-gutter)
                 piles
                 (let [first-spacing (- (first old-ys) half-gutter)]
                   (reduce (fn [piles [y y']] (move-row piles y y'))
                           piles
                           (map vector
                                old-ys
                                (map #(- % first-spacing) old-ys)))))
        ys (keys piles')
        spacings (row-spacings ys)
        spacings' (for [row (butlast (vals piles'))]
                    (+ (apply max (map pile-height (vals row))) gutter))
        ys' (reductions + half-gutter spacings')]
    (if (= spacings spacings')
      piles'
      (reduce (fn [piles' [y y']] (move-row piles' y y'))
              piles'
              (map vector ys ys')))))

;;
;; Dragging
;;

(def drag-x-offset (/ card-width 2))
(def drag-y-offset (-> (* 0.4 card-height) int))

(defn drag-pile-pos
  "Given the mouse's x & y coordinates, return the position of the drag pile
  such that the mouse is in the center."
  [x y]
  [(- x drag-x-offset)
   (- y drag-y-offset)])

(defn mouse-pos
  [drag-x drag-y]
  [(+ drag-x drag-x-offset)
   (+ drag-y drag-y-offset)])

(defn distance-squared-to
  ([p] (let [{x :x y :y} p] (distance-squared-to x y)))
  ([x y]
   (fn [[cx cy]]
     (let [dx (- x (+ cx half-card-width))
           dy (- y (+ cy half-card-height))]
       (+ (* dx dx) (* dy dy))))))

(defn drag-target
  [drag piles]
  ;; want to find the nearest drop position for the drag, which means:
  ;;   nearest pile or place where new pile could be inserted
  (if drag
    (let [[x y] (mouse-pos (:x drag) (:y drag))
          col-index (quot (- x half-gutter) pile-spacing)
          left-col (-> (* col-index pile-spacing) (+ half-gutter))
          right-col (-> (* (inc col-index) pile-spacing) (+ half-gutter))
          rows (vals piles)
          all-piles (mapcat vals rows)
          row-ys (keys piles)
          row-count (count piles)
          last-row-height (row-height (get piles (last row-ys)))
          row-spacings (conj (vec (row-spacings row-ys))
                             last-row-height)
          [before-and-on after] (split-with #(<= % y) row-ys)
          first-row-y half-gutter
          row-y (or (last before-and-on) first-row-y)
          curr-row-height (cond
                            (= row-count 1) (first row-spacings)
                            (< y first-row-y) (first row-spacings)
                            (not (empty? after)) (nth row-spacings
                                                      (dec (count before-and-on)))
                            :otherwise last-row-height)
          candidates (for [cx [left-col right-col]
                           cy [row-y (+ row-y curr-row-height)]]
                       [cx cy])]
      (first (sort-by (distance-squared-to x y) candidates)))))

;;
;; Saving & Loading State
;;

(defn state-key
  [pack-spec seed]
  (str pack-spec "|" seed))

(defn path-components
  [page-path]
  (-> page-path
    (str/replace #"^/" "")
    (str/split #"/")))

(defn save-state!
  [state]
  (let [page-path (-> js/document .-location .-pathname)
        [ps seed] (path-components page-path)
        pool-key (state-key ps seed)
        ts (-> (js/Date.) .getTime (/ 1000) int str)
        clean-state (-> state
                      (dissoc :drag)
                      (dissoc :selection))
        serialized (str ts (pr-str clean-state))]
    (try
      (.setItem js/localStorage pool-key serialized)
      (catch js/Error e
        (if (= (.-name e) "QuotaExceededError")
          (do
            (.cleanStorage js/window)
            (.setItem js/localStorage pool-key serialized))
          (throw e))))))

(defn load-state
  [pack-spec seed]
  (if-let [saved-state (.getItem js/localStorage (state-key pack-spec seed))]
    (let [unsorted-state (reader/read-string (.substr saved-state ts-digits))]
      {:piles (into (sorted-map) (for [[y row] (:piles unsorted-state)]
                                   [y (into (sorted-map) (for [[x pile] row]
                                                           [x pile]))]))})))

;;
;; State Manipulation
;;

(def !fate (atom {:past [], :future ()}))

(defn add-to-history-if-new!
  [state]
  (swap! !fate (fn [{:keys [past] :as fate}]
                 (if (= state (last past))
                   fate
                   (-> fate
                     (update-in [:past] (fnil conj []) state)
                     (assoc :future ())))))
  (save-state! state)
  state)

(defn do-rewind
  [fate]
  (let [{:keys [past]} fate]
    (if (<= (count past) 1)
      fate
      (-> fate
        (update-in [:past] (comp vec butlast))
        (update-in [:future] (fnil conj ()) (last past))))))

(defn rewind!
  [current]
  (let [fate' (swap! !fate do-rewind)]
    (-> fate' :past last)))

(defn do-skip
  [fate]
  (if-let [new-present (-> fate :future first)]
    (-> fate
      (update-in [:future] rest)
      (update-in [:past] (fnil conj []) new-present))
    fate))

(defn skip-ahead!
  [current]
  (let [fate' (swap! !fate do-skip)]
    (-> fate' :past last)))

;;
;; Signal Graph's State Actions
;;

(defn start-drag-action
  "Start a drag if:
    * there are selected cards and the click is on a pile with selected cards
      (but not above the selected cards in that pile);
    * there are no selected cards but the click is on some card (start dragging
      just that card).
  Returns the state with or without a newly-started drag, depending on if the
  above criteria are satisfied. "
  [pos]
  (fn [{:keys [piles] :as state}]
    (let [piles-in-selection (filter #(some :selected? (:cards %))
                                     (mapcat vals (vals piles)))
          [x y] pos
          extant-selection-drag? (loop [ps piles-in-selection]
                                   (if-let [{l :x, pt :y, cards :cards, :as p} (first ps)]
                                     (let [r (+ l card-width)
                                           t (-> (filter :selected? cards) first :y)
                                           b (+ pt (pile-height p))]
                                       (if (within? l r t b x y)
                                         true
                                         (recur (next ps))))
                                     ;; else (no more piles)
                                     false))
          card-under-mouse (card-under piles x y)]

      (cond
        extant-selection-drag?
        (let [[drag-pile-x drag-pile-y] (drag-pile-pos x y)
              selected-cards (filter :selected?
                                     (mapcat :cards piles-in-selection))]
          (-> (reduce (fn [state {px :x, py :y, cards :cards}]
                        (let [cards' (remove :selected? cards)]
                          (if (empty? cards')
                            (remove-pile state px py)
                            (add-pile state (make-pile cards' px py)))))
                      state
                      piles-in-selection)
            (assoc :drag (make-pile selected-cards drag-pile-x drag-pile-y))))

        card-under-mouse ;; make drag pile w/only current card
        (let [[dpx dpy] (drag-pile-pos x y)
              {px :x, py :y :as pile} (pile-under piles x y)
              pile-cards' (remove #(= (:id %) (:id card-under-mouse)) (:cards pile))]
          (-> (if (empty? pile-cards')
                (remove-pile state px py)
                (add-pile state (make-pile pile-cards' px py)))
            (assoc :drag (make-pile [card-under-mouse] dpx dpy))))

        :otherwise (dissoc state :drag)))))

(defn apply-selection
  [state selection]
  (let [{piles :piles} state]
    (if-not selection
      state
      (reduce (fn [state {x :x, y :y, :as pile'}]
              (assoc-in state [:piles y x] pile'))
            state
            (map (partial pile-after-selection selection)
                 (mapcat vals (vals piles)))))))

(defn start-selection-if-not-dragging-action
  [pos]
  (fn [state]
    (if (:drag state)
      state
      (let [selection {:start pos, :stop pos}]
        (-> state
          (apply-selection selection)
          (assoc :selection selection))))))

(defn update-selection-or-drag-destination-action
  [pos]
  (fn [{:keys [drag selection piles] :as state}]
    (let [max-pile-x (or (:max-pile-x state) (max-pile-x piles))
          max-x (+ max-pile-x card-width gutter
                   (if drag (+ card-width half-card-width) 0))
          x (min max-x (nth pos 0))
          y (nth pos 1)]
      (cond
        drag (let [[px py] (drag-pile-pos x y)]
               (assoc state :drag (make-pile (:cards drag) px py)))
        selection (update-in state [:selection] assoc :stop [x y])
        :else state))))

(defn add-max-pile-x
  [state]
  (assoc state :max-pile-x (max-pile-x (:piles state))))

(defn stop-selection-or-drag-action
  [_]
  (fn [{:keys [selection drag piles] :as state}]
    (cond
      selection (-> state
                  (apply-selection selection)
                  (dissoc :selection))
      drag (let [[tx ty] (drag-target drag piles)
                 new-pile (if-let [{old-cards :cards} (get-in piles [ty tx])]
                            (make-pile (concat old-cards (:cards drag))
                                       tx, ty)
                            (make-pile (:cards drag) tx ty))]
             (-> state
               (add-pile new-pile)
               (update-in [:piles] rejigger-rows)
               (dissoc :drag)
               add-max-pile-x
               (add-to-history-if-new!)))
      :otherwise state)))

(defn rewind-state-action
  [should-rewind?]
  (if should-rewind?
    (fn [current] (rewind! current))
    identity))

(defn skip-ahead-state-action
  [_]
  (fn [current] (skip-ahead! current)))

;;
;; Signal Graph
;;

(defn listen
  [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

(defn undo-channel
  [_ _]
  (listen (.getElementById js/document "undo-button")
          "click"
          (map (constantly true))))

(defn redo-channel
  [_ _]
  (listen (.getElementById js/document "redo-button")
          "click"
          (map (constantly true))))

(defn on-key-code-down
  [code]
  (sig/keep-if identity true (sig/drop-repeats (keys/down? code))))

(defn state-signal
  [initial-state]
  (let [app-mouse-position (sig/map #(update-in % [1] - mouse-y-offset) mouse/position)
        drag-coords (sig/keep-when mouse/down? [0 0] app-mouse-position)
        dragging? (let [true-on-dragmove (sig/sample-on drag-coords (sig/constant true))]
                    (->> (sig/merge (sig/keep-if not false mouse/down?) true-on-dragmove)
                      sig/drop-repeats))
        start-drag (sig/keep-if identity true dragging?)
        stop-drag (sig/keep-if not false dragging?)
        drag-start-coords (sig/sample-on start-drag app-mouse-position)
        click-down (sig/keep-if identity true (sig/drop-repeats mouse/down?))
        click-down-coords (sig/sample-on click-down app-mouse-position)
        click-up (sig/keep-if not false (sig/drop-repeats mouse/down?))
        undo-button-down (sig/input false :undo-button undo-channel)
        redo-button-down (sig/input false :redo-button redo-channel)
        actions (sig/merge
                  (sig/map start-drag-action click-down-coords)
                  (sig/map start-selection-if-not-dragging-action drag-start-coords)
                  (sig/map update-selection-or-drag-destination-action drag-coords)
                  (sig/map stop-selection-or-drag-action stop-drag)
                  (sig/map stop-selection-or-drag-action click-up)
                  (sig/map rewind-state-action (on-key-code-down u-key-code))
                  (sig/map rewind-state-action undo-button-down)
                  (sig/map skip-ahead-state-action (on-key-code-down r-key-code))
                  (sig/map skip-ahead-state-action redo-button-down)
                  (sig/constant identity))]
    (sig/drop-repeats
      (sig/reductions (fn [state action] (action state))
                      initial-state
                      actions))))

;;
;; Rendering
;;

(defn render-selection
  [state]
  (if-let [selection (:selection state)]
    (let [{:keys [piles-after-selection]} state
          selected-count (->> piles-after-selection
                           (mapcat :cards)
                           (filter :selected?)
                           count)
          [ox oy] (:stop selection)
          [left right top bottom] (selection-edges selection)]

      (dom/div nil
               (dom/div #js {:id "selection"
                             :className "box"
                             :style #js {:position "absolute"
                                         :top top
                                         :left left
                                         :width (- right left)
                                         :height (- bottom top)}})

               (if (pos? selected-count)
                 (dom/div #js {:id "counter"
                               :className "badge"
                               :style #js {:position "absolute"
                                           :top (- oy (* 1.25 em))
                                           :left (- ox (* 4 em))}}
                          (pr-str selected-count)))))))

(defn render-card
  ([card] (render-card card 0 0))
  ([card dx dy]
   (let [{:keys [name img-src x y selected?]} card]
     (dom/div #js {:className (str "card" (if selected? " selected"))
                   :style #js {:position "absolute"
                               :left (+ x dx)
                               :top (+ y dy)}}
              (dom/img #js {:src img-src, :title name
                            :width card-width, :height card-height})))))

(defn render-pile
  [pile]
  (apply dom/div #js {:className "pile"}
         (map render-card (:cards pile))))

(defn render-drag
  [drag piles]
  (if drag
    (let [[tx ty] (drag-target drag piles)]
      (dom/div nil
               (dom/div #js {:id "drag-target"
                             :className "ghost"
                             :style #js {:position "absolute"
                                         :left tx
                                         :top ty
                                         :width card-width
                                         :height card-height}})
               (apply dom/div #js {:id "drag" :className "pile"}
                      (map render-card (:cards drag)))))))

(defn render-dfc
  [state]
  (let [drag-cards (get-in state [:drag :cards] [])
        {dx :x dy :y} (:drag state)
        other-sides (map :reverse drag-cards)]
    (if (seq (filter :dfc drag-cards))
      (apply dom/div #js {:className "backsides-holder"
                          :style #js {:position "absolute"
                                      :left (+ dx card-width), :top dy}}
             (map-indexed (fn [i card]
                            (if card (render-card card 0 (* i pile-stride))))
                          other-sides)))))

(defn preload-dfcs
  [state]
  (let [dfc-cards (->> (concat (filter :dfc (state->cards state))
                               (filter :dfc (get-in state [:drag :cards])))
                    (sort-by :name))]
    (apply dom/div #js {:className "dfc-preloader"
                        :style #js {:display "none"}}
           (for [{:keys [img-src]} (map :reverse dfc-cards)]
             (dom/img #js {:src img-src
                           :style #js {:display "block"}})))))

(defn render-hud
  [state]
  (let [w-dragged-ids (if (contains? state :drag)
                        (update-in state [:drag :cards] (partial map :id))
                        state)
        no-piles (dissoc w-dragged-ids :piles)]
    (dom/div #js {:id "hud", :style #js {:position "relative"}}
             (dom/pre nil
                      (dom/b nil
                             (.stringify js/JSON
                               (clj->js no-piles) nil 2))))))

(defn render-footer
  [state]
  (let [{:keys [drag piles]} state
        max-y (+ (apply max (keys piles))
                 (row-height (get piles (last (keys piles)))))]
    (dom/div #js {:id "footer"
                  :style #js {:position "absolute"
                              :top max-y}}
             (dom/div #js {:className "disclaimer"}
                      (dom/strong nil "Magic: the Gathering")
                      " is © Wizards of the Coast"
                      " • Lab Maniac is in no way affiliated with Wizards of the Coast")
             (dom/div #js {:className "disclaimer"}
                      "Data from " (dom/a #js {:href "http://mtgjson.com"} "mtgjson.com")
                      ", images from " (dom/a #js {:href "http://magiccards.info"} "magiccards.info & Gatherer")
                      " • Made by Dan Lidral-Porter"))))

(defn render-state
  [state]
  (let [{selection :selection} state
        piles (->> (-> state :piles vals)
                (mapcat vals)
                (map (partial pile-after-selection selection)))]
    (dom/div #js {:id "dom-root"}
             (apply dom/div {:id "piles"} (map render-pile piles))
             (render-drag (:drag state) (:piles state))
             (render-selection (assoc state :piles-after-selection piles))
             (render-dfc state)
             (render-footer state)
             (preload-dfcs state))))

(defn- navigate!
  "Redirect to the given sealed format."
  [format-str]
  (set! (.-location js/document) (str "/" format-str)))

(defn render-navigator
  ([all-sets] (render-navigator all-sets nil))
  ([all-sets current-format]
   (let [valid-sets (filter (comp sets/sets-that-work keyword :code) all-sets)
         known-formats (into #{} (map :sealed-format valid-sets))
         custom-format? (not (contains? known-formats current-format))
         options (if-not custom-format?
                   valid-sets
                   (conj valid-sets
                         {:code "custom"
                          :name (str "Custom - " current-format)
                          :release-date "9999-99-99"}))]
     (dom/span #js {}
     "Change Format: "
     (dom/select #js {:className "om-selector"
                      :name "set"
                      :defaultValue (or current-format "6KLD")
                      :onChange (fn [event]
                                  (navigate! (-> event .-target .-value)))}
                 (for [{:as mtg-set
                        :keys [code sealed-format]} (->> options
                                                      (sort-by :release-date)
                                                      reverse)]
                   (dom/option #js {:value sealed-format
                                    :id (str "select-set-option-" code)}
                               (:name mtg-set))))))))


;;
;; Interface Hackery
;;
;; TODO: render page server-side so this doesn't have to be done here
;;

(defn transform-to-deck-ui!
  []
  (let [button (.getElementById js/document "new-pool-button")]
    (set! (.-href button) "/")
    (set! (.-innerText button) "Random Pool")))

;;
;; App Setup
;;

(defn start-om
  [state]
  (om/root
    (fn [app owner]
      (reify om/IRender
        (render [_]
          (render-state app))))
    state
    {:target (.getElementById js/document "app")}))

(defn start-navigator
  "Given the target id for the navigator element, a sequence with every set's
  metadata and optionally the current format, create the site navigator."
  ([target-id all-sets]
   (start-navigator target-id all-sets ::no-format))
  ([target-id all-sets current-format]
   (om/root
     (fn [app owner]
       (reify om/IRender
         (render [_]
           (render-navigator
             all-sets
             (if (not= current-format ::no-format) current-format)))))
     all-sets
     {:target (.getElementById js/document (name target-id))})))

(defn start-app-from-state!
  [init-state]
  (let [init-state (add-max-pile-x init-state)
        state-atom (sig/pipe-to-atom (state-signal init-state))]
    (swap! !fate update-in [:past] (fnil conj ()) init-state)
    (start-om state-atom)))

(def blank-state {:piles (sorted-map)})

(defn sealed-pool-piles
  [cards]
  (let [cards (remove basic-land? cards)
        [rares others] ((juxt (partial filter rare?)
                              (partial remove rare?))
                        cards)
        rare->pile (fn [i rare]
                     (make-pile [rare] (x-of-column-indexed i) half-gutter))
        rare-piles (map-indexed rare->pile (sort-by wubrggc-sort rares))
        color->non-rares (group-by (fn [{:keys [colors manaCost] :as card}]
                                     (if-let [cost-color (cost->colortype manaCost)]
                                       cost-color
                                       (cond
                                         (empty? colors) :colorless
                                         (> (count colors) 1) :gold
                                         :otherwise (first colors))))
                                   others)
        colors-and-xs (reduce (fn [cs-&-ps color]
                                (let [last-x (-> cs-&-ps
                                               (nth (dec (count cs-&-ps)) nil)
                                               (nth 1 nil))
                                      x (if last-x
                                          (+ last-x pile-spacing)
                                          half-gutter)]
                                  (if (empty? (color->non-rares color))
                                    cs-&-ps
                                    (conj cs-&-ps [color x]))))
                              []
                              color-order)
        pile-for-color-at-x (fn [[color x]]
                              (let [col-cards (color->non-rares color)
                                    y (+ half-gutter card-height gutter)]
                                (make-pile (sort-by :name col-cards) x y)))
        color-piles (map pile-for-color-at-x colors-and-xs)]
    (concat rare-piles color-piles)))

(defn deck-piles
  [cards]
  (map-indexed (fn [i [_ cmc-cards]]
                 (make-pile (sort-by :name cmc-cards) (x-of-column-indexed i) half-gutter))
               (sort-by key (group-by :cmc cards))))

(defn api-cards->init-state
  [api-cards]
  (let [cards (map api-card->client-card api-cards)
        piles (if (> (count cards) 76)
                (sealed-pool-piles cards)
                (deck-piles cards))]
        (reduce (fn [state pile] (add-pile state pile)) blank-state piles)))

(defn start-app-from-api-cards!
  [api-cards]
  (let [init-state (api-cards->init-state api-cards)]
    (start-app-from-state! init-state)))

(defn halt-app-on-err
  [{:keys [status], {:keys [msg kind]} :response}]
  (if (= status 400)
    (js/alert (str msg " Please edit the packs parameter in the query string"
                   " accordingly and then hit enter."))
    (js/alert "Could not communicate with the backend server."
              " Please try again later")))

(defn log-error
  [{:keys [status], {:keys [msg kind] :as response} :response, :as err}]
  (println "got a" status msg "error:" kind)
  (println (keys err))
  (println (:failure err) (:status-text err)))

(def default-pack-spec "6KTK")

(defn get-sets-and-start-navigator!
  []
  ;; The format from the URL is attacker-controlled , so we have to be very
  ;; careful how we handle it in order to avoid getting owned. We'll only accept
  ;; sealed formats from URLs if they consist of repeated four-character
  ;; segments that each have a number followed by three alphanumeric characters.
  (let [url-format (-> js/document
                     .-location
                     .-pathname
                     (.split "/")
                     (aget 1)
                     str/upper-case)
        current-format (if (re-matches #"([0-9][0-9A-Z]{3})+" url-format)
                         url-format)]
    (println current-format)
    (async-http/GET "/api/sets?booster-only=1"
                    {:response-format (edn-response-format)
                     :handler (if current-format
                                #(start-navigator "navigator" % current-format)
                                #(start-navigator "navigator" %))
                     :error-handler log-error})))

(defn get-state-and-start-app!
  []
  (let [page-path (-> js/document .-location .-pathname)
        components (path-components page-path)]
    (if (= (first components) "decks")
      (let [[_ deck-hash] components]
        (transform-to-deck-ui!)
        (async-http/GET (str "/api/decks/" deck-hash)
                        {:response-format (edn-response-format)
                         :handler start-app-from-api-cards!
                         :error-handler halt-app-on-err}))
      ;; otherwise, if we're not loading a deck, we're in the sealed section
      (let [[pack-spec seed] components]
        (if-let [saved-state (load-state pack-spec seed)]
          (start-app-from-state! saved-state)
          (async-http/GET (str "/api/pool/" (or pack-spec default-pack-spec) "/" seed)
                          {:response-format (edn-response-format)
                           :handler start-app-from-api-cards!
                           :error-handler halt-app-on-err}))))))

(get-state-and-start-app!)
(get-sets-and-start-navigator!)

;;
;; Help Button
;;

(events/listen
  (.getElementById js/document "help-button")
  "click"
  (fn [e]
    (let [meta-div (.getElementById js/document "meta")
          display? (not= (-> meta-div .-style .-display) "none")]
      (if display?
        (set! (-> meta-div .-style .-display) "none")
        (set! (-> meta-div .-style .-display) "")))))

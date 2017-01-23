(ns arcane-lab.main
  (:require [ajax.core :as async-http]
            [ajax.edn :refer [edn-response-format]]
            [arcane-lab.constants :as c]
            [arcane-lab.drag :as drag]
            [arcane-lab.geom :refer [within? between?]]
            [arcane-lab.history :as history]
            [arcane-lab.piles :as piles]
            [arcane-lab.sets :as sets]
            [arcane-lab.state :as state]
            [cljs-uuid-utils.core :refer [make-random-uuid]]
            [cljs.core.async :as async]
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
                      :x c/half-gutter, :y c/half-gutter
                      :selected? false)
      reverse-side (update :reverse add-img-src))))

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
;; Signal Graph's State Actions
;;

(defn start-drag-action
  "Start a drag if:
    * there are selected cards and the click is on a pile with selected cards
      (but not above the selected cards in that pile);
    * there the click is on some unselected card (start dragging
      just that card, regardless of whether or not there's a selection).
  Returns the state with or without a newly-started drag, depending on if the
  above criteria are satisfied. "
  [pos]
  (fn [{:keys [piles] :as state}]
    (let [piles-in-selection (filter #(some :selected? (:cards %))
                                     (state/state->piles state))
          [x y] pos
          extant-selection-drag? (loop [ps piles-in-selection]
                                   (if-let [{l :x, pt :y, cards :cards, :as p} (first ps)]
                                     (let [r (+ l c/card-width)
                                           t (-> (filter :selected? cards) first :y)
                                           b (+ pt (piles/pile-height p))]
                                       (if (within? l r t b x y)
                                         true
                                         (recur (next ps))))
                                     ;; else (no more piles)
                                     false))
          card-under-mouse (piles/card-under piles x y)
          [dx dy] (drag/drag-pile-pos x y)
          drag-target (drag/drag-target {:x dx, :y dy} piles)]

      (cond
        extant-selection-drag?
        (let [[drag-pile-x drag-pile-y] (drag/drag-pile-pos x y)
              selected-cards (filter :selected?
                                     (mapcat :cards piles-in-selection))
              drag-pile (piles/make-drag-pile selected-cards
                                              drag-pile-x, drag-pile-y)]
          (-> (reduce (fn [state {px :x, py :y, cards :cards}]
                        (let [cards' (remove :selected? cards)]
                          (if (empty? cards')
                            (state/remove-pile state px py)
                            (state/add-pile state
                                            (piles/make-pile cards' px py)))))
                      state
                      piles-in-selection)
            (assoc :drag drag-pile
                   :drag-target drag-target)))

        card-under-mouse ;; make drag pile w/only current card
        (let [[dpx dpy] (drag/drag-pile-pos x y)
              {px :x, py :y :as pile} (piles/pile-under piles x y)
              pile-cards' (remove #(= (:id %) (:id card-under-mouse))
                                  (:cards pile))]
          (-> (if (empty? pile-cards')
                (state/remove-pile state px py)
                (state/add-pile state (piles/make-pile pile-cards' px py)))
            (assoc :drag (piles/make-drag-pile [card-under-mouse] dpx dpy)
                   :drag-target drag-target)))

        :otherwise (dissoc state :drag)))))

(defn start-selection-if-not-dragging-action
  [pos]
  (fn [state]
    (if (:drag state)
      state
      (let [selection {:start pos, :stop pos}]
        (-> state
          (state/apply-selection selection)
          (assoc :selection selection))))))

(defn update-selection-or-drag-destination-action
  [pos]
  (fn [{:keys [drag selection piles] :as state}]
    (let [max-pile-x (or (:max-pile-x state) (piles/max-pile-x piles))
          max-x (+ max-pile-x c/card-width c/gutter
                   (if drag (+ c/card-width c/half-card-width) 0))
          x (min max-x (nth pos 0))
          y (nth pos 1)]
      (cond
        drag (state/update-drag state x y)
        selection (state/update-selection state x y)
        :else state))))

(defn stop-selection-or-drag-action
  [_]
  (fn [{:keys [selection drag piles] :as state}]
    (cond
      selection (-> state
                  (state/apply-selection selection)
                  (dissoc :selection))
      drag (let [[tx ty] (drag/drag-target drag piles)
                 new-pile (if-let [{old-cards :cards} (state/get-pile state tx ty)]
                            (piles/make-pile (concat old-cards (:cards drag))
                                             tx, ty)
                            (piles/make-pile (:cards drag) tx ty))]
             (-> state
               (state/add-pile new-pile)
               (update-in [:piles] piles/rejigger-rows)
               (dissoc :drag :drag-target)
               state/add-max-pile-x
               state/add-selection-triggers
               state/add-drop-zones
               history/add-to-history-if-new!))
      :otherwise state)))

(defn rewind-state-action
  [should-rewind?]
  (if should-rewind?
    (fn [current] (history/rewind! current))
    identity))

(defn skip-ahead-state-action
  [_]
  (fn [current] (history/skip-ahead! current)))

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
  (let [app-mouse-position (sig/map #(update-in % [1] - c/mouse-y-offset) mouse/position)
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
                  (sig/map rewind-state-action (on-key-code-down c/u-key-code))
                  (sig/map rewind-state-action undo-button-down)
                  (sig/map skip-ahead-state-action (on-key-code-down c/r-key-code))
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
    (let [selected-count (->> (state/state->cards state)
                           (filter :selected?)
                           count)
          [ox oy] (:stop selection)
          [left right top bottom] (piles/selection-edges selection)]

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
                                           :top (- oy (* 1.25 c/em))
                                           :left (- ox (* 4 c/em))}}
                          (pr-str selected-count)))))))

(defn render-card
  ([card] (render-card card 0 0))
  ([card dx dy]
   (let [{:keys [name id img-src x y selected?]} card]
     (dom/div #js {:className (str "card" (if selected? " selected"))
                   :style #js {:left (+ x dx)
                               :top (+ y dy)}
                   :key id}
              (dom/img #js {:src img-src, :title name
                            :width c/card-width
                            :height c/card-height})))))

(defn render-pile
  [pile]
  (dom/div #js {:className "pile"}
           (map #(render-card % 0 0) (:cards pile))))

(defn render-drag
  [state]
  (if-let [drag (:drag state)]
    (let [[tx ty] (:drag-target state)
          target-pile (state/get-pile state tx ty)
          target-height (if target-pile
                          (piles/pile-height target-pile)
                          c/card-height)]
      (dom/div nil
               (dom/div #js {:id "drag-target"
                             :className "ghost"
                             :style #js {:position "absolute"
                                         :left tx
                                         :top ty
                                         :width c/card-width
                                         :height target-height}})
               (apply dom/div #js {:id "drag" :className "pile"}
                      (map render-card (:cards drag)))))))

(defn render-dfc
  [state]
  (if (get-in state [:drag :dfcs?])
    (let [{dx :x dy :y} (:drag state)]
      (apply dom/div #js {:className "backsides-holder"
                          :style #js {:position "absolute"
                                      :left (+ dx c/card-width), :top dy}}
             (map-indexed (fn [i card]
                            (if card
                              (render-card card 0 (* i c/pile-stride))))
                          (map :reverse (get-in state [:drag :cards])))))))

(defn preload-dfcs
  [state]
  (if-let [dfcs (seq (:dfcs state))]
    (dom/div #js {:className "dfc-preloader"}
             (for [{:keys [img-src]} (map :reverse dfcs)]
               (dom/img #js {:src img-src
                             :style #js {:display "block"}})))))

(defn render-hud
  [state]
  (let [trimmed-state (-> (if (contains? state :drag)
                            (update-in state [:drag :cards] (partial map (comp str :id)))
                            state)
                        (update :dfcs (partial map (comp str :id)))
                        (dissoc :piles :selection-triggers))]
    (dom/div #js {:id "hud"}
             (dom/pre nil
                      (dom/b nil
                             (.stringify js/JSON
                               (clj->js trimmed-state) nil 2))))))

(defn render-footer
  [state]
  (let [{:keys [drag piles]} state
        max-y (+ (apply max (keys piles))
                 (piles/row-height (get piles (last (keys piles)))))] ;; TODO replace w/last-row fn
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
  (dom/div #js {:id "dom-root"}
           (apply dom/div {:id "piles"}
                  (map render-pile (state/state->piles state)))
           (render-drag state)
           (render-selection state)
           (render-dfc state)
           (render-footer state)
           (preload-dfcs state)))

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
                                                        reverse)
                         :let [id (str "select-set-option-" code)]]
                     (dom/option #js {:value sealed-format
                                      :id id, :key id}
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

(defn migrate-state
  [possibly-old-state]
  (let [w-cached-vals (-> possibly-old-state
                        state/add-max-pile-x
                        state/add-selection-triggers
                        state/add-drop-zones
                        state/add-dfcs)]
    (state/map-piles #(piles/make-pile (:cards %) (:x %) (:y %))
                     w-cached-vals)))

(defn start-app-from-state!
  [init-state]
  (let [state (migrate-state init-state)
        state-atom (sig/pipe-to-atom (state-signal state))]
    (swap! history/!fate update-in [:past] (fnil conj ()) state)
    (start-om state-atom)))

(def blank-state {:piles (sorted-map)})

(defn sealed-pool-piles
  [cards]
  (let [cards (remove basic-land? cards)
        [rares others] ((juxt (partial filter rare?)
                              (partial remove rare?))
                        cards)
        rare->pile (fn [i rare]
                     (piles/make-pile
                       [rare] (piles/x-of-column-indexed i) c/half-gutter))
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
                                          (+ last-x c/pile-spacing)
                                          c/half-gutter)]
                                  (if (empty? (color->non-rares color))
                                    cs-&-ps
                                    (conj cs-&-ps [color x]))))
                              []
                              color-order)
        pile-for-color-at-x (fn [[color x]]
                              (let [col-cards (color->non-rares color)
                                    y (+ c/half-gutter c/card-height c/gutter)]
                                (piles/make-pile (sort-by :name col-cards) x y)))
        color-piles (map pile-for-color-at-x colors-and-xs)]
    (concat rare-piles color-piles)))

(defn deck-piles
  [cards]
  (map-indexed (fn [i [_ cmc-cards]]
                 (piles/make-pile (sort-by :name cmc-cards)
                                  (piles/x-of-column-indexed i) c/half-gutter))
               (sort-by key (group-by :cmc cards))))

(defn api-cards->init-state
  [api-cards]
  (let [cards (map api-card->client-card api-cards)
        piles (if (> (count cards) 76)
                (sealed-pool-piles cards)
                (deck-piles cards))]
        (reduce (fn [state pile] (state/add-pile state pile)) blank-state piles)))

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
        components (history/path-components page-path)]
    (if (= (first components) "decks")
      (let [[_ deck-hash] components]
        (transform-to-deck-ui!)
        (async-http/GET (str "/api/decks/" deck-hash)
                        {:response-format (edn-response-format)
                         :handler start-app-from-api-cards!
                         :error-handler halt-app-on-err}))
      ;; otherwise, if we're not loading a deck, we're in the sealed section
      (let [[pack-spec seed] components]
        (if-let [saved-state (history/load-state pack-spec seed)]
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

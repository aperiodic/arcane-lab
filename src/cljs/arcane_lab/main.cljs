(ns arcane-lab.main
  (:require [ajax.core :as async-http]
            [ajax.edn :refer [edn-response-format]]
            [arcane-lab.card :as card]
            [arcane-lab.color :as color]
            [arcane-lab.constants :as c]
            [arcane-lab.history :as history]
            [arcane-lab.piles :as piles]
            [arcane-lab.render :as render]
            [arcane-lab.signal :as signal]
            [arcane-lab.state :as state]
            [cljs-uuid-utils.core :refer [make-random-uuid]]
            [clojure.string :as str]
            [goog.events :as events]
            [jamesmacaulay.zelkova.signal :as sig]
            [js.imagesloaded]
            [om.core :as om :include-macros true]))

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

;; I can't figure out how to put other things in the root om atom since Zelkova
;; seems to obliterate anything that doesn't come from its signals, hence this
;; ugly hack of a seperate atom only to track whether to render the loading GIF
(def !loaded-hack (atom false))

(defn remove-loader
  [state]
  (reset! !loaded-hack true)
  (om/transact! state :images-loaded? (constantly true))
  (println "images all loaded!"))

(defn start-om
  [state]
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (render/cards app !loaded-hack))
        om/IDidMount
        (did-mount [_]
          (js/imagesLoaded "div#app" #(remove-loader app)))))
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
           (render/navigator
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
        state-atom (sig/pipe-to-atom (signal/drag-drop-select-undo-redo state))]
    (swap! history/!fate update-in [:past] (fnil conj ()) state)
    (start-om state-atom)))

(def blank-state {:piles (sorted-map)})

(defn card->sort-color
  [card]
  (if (:dfc? card)
    (color/id->category (:colors card))
    (color/id->category (:color-identity card))))

(defn sealed-pool-piles
  [cards]
  (let [cards (remove card/basic-land? cards)
        [rares others] ((juxt (partial filter card/rare?)
                              (partial remove card/rare?))
                        cards)
        rare->pile (fn [i rare]
                     (piles/make-pile
                       [rare] (piles/x-of-column-indexed i) c/half-gutter))
        rare-piles (map-indexed rare->pile (sort-by color/wubrggc-sort rares))
        color->non-rares (group-by card->sort-color others)
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
                              color/categories)
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

(ns arcane-lab.main
  (:require [ajax.core :as async-http]
            [ajax.edn :refer [edn-response-format]]
            [arcane-lab.action :as action]
            [arcane-lab.card :as card]
            [arcane-lab.color :as color]
            [arcane-lab.constants :as c]
            [arcane-lab.history :as history]
            [arcane-lab.piles :as piles]
            [arcane-lab.render :as render]
            [arcane-lab.state :as state]
            [arcane-lab.state-machine :refer [drag-select-scan-cards]]
            [arcane-lab.ui :as ui]
            [arcane-lab.useful :refer [rand-uuid]]
            [cljs.core.async :as async]
            [clojure.string :as str]
            [cypress.core :as cypress]
            [goog.events :as events]
            [js.imagesloaded]
            [om.core :as om :include-macros true]))

(enable-console-print!)

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
;; Loader Cleanup
;;
;; We want to keep the loader until all the images have loaded, which is well
;; after the app starts working on a cold load (i.e. for a new visitor).
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

;;
;; DOM Hooks
;;

(defn app-root-node
  []
  (.getElementById js/document "app"))

(defn navigator-node
  []
  (.getElementById js/document "navigator"))

(defn- click-chan
  [element sigil]
  (let [out (async/chan 4)]
    (.addEventListener element "mousedown"
      (fn [e]
        (.preventDefault e)
        (.stopPropagation e)
        (async/put! out sigil))
      #js {:passive false})
    out))

(defn undo-button-down-chan
  []
  (click-chan (.getElementById js/document "undo-button") :arcane-lab/undo))

(defn redo-button-down-chan
  []
  (click-chan (.getElementById js/document "redo-button") :arcane-lab/redo))

(defn custom-cypress-events
  []
  {:arcane-lab/undo (undo-button-down-chan)
   :arcane-lab/redo (redo-button-down-chan)})

;;
;; App Setup
;;

(defn start!
  [!state]
  (cypress/init!
    js/document (custom-cypress-events) drag-select-scan-cards !state)
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (render/cards app !loaded-hack))
        om/IDidMount
        (did-mount [_]
          (js/imagesLoaded "div#app" #(remove-loader app)))))
    !state
    {:target (app-root-node)}))

(defn start-navigator!
  "Given the target id for the navigator element, a sequence with every set's
  metadata and optionally the current format, create the site navigator."
  ([all-sets]
   (start-navigator! all-sets ::no-format))
  ([all-sets current-format]
   (om/root
     (fn [app owner]
       (reify om/IRender
         (render [_]
           (render/navigator
             all-sets
             (if (not= current-format ::no-format) current-format)))))
     all-sets
     {:target (navigator-node)})))

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
  (let [state (migrate-state init-state)]
    (swap! history/!fate update-in [:past] (fnil conj ()) state)
    (start! (atom state))))

(def blank-state {:piles (sorted-map)})

(defn start-app-from-piles!
  [initial-piles]
  (start-app-from-state! (reduce (fn [state pile] (state/add-pile state pile))
                                 blank-state
                                 initial-piles)))

(defn start-app-from-deck!
  [deck-cards]
  (start-app-from-piles! (ui/deck->piles deck-cards)))

(defn start-app-from-pool!
  [pool-cards-by-rarity]
  (start-app-from-piles! (ui/pool->piles pool-cards-by-rarity)))

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
                                #(start-navigator! % current-format)
                                #(start-navigator! %))
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
                         :handler start-app-from-deck!
                         :error-handler halt-app-on-err}))
      ;; otherwise, if we're not loading a deck, we're in the sealed section
      (let [[pack-spec seed] components]
        (if-let [saved-state (history/load-state pack-spec seed)]
          (start-app-from-state! saved-state)
          (async-http/GET (str "/api/pool/" (or pack-spec default-pack-spec)
                               "/" seed
                               "?structured=1")
                          {:response-format (edn-response-format)
                           :handler start-app-from-pool!
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

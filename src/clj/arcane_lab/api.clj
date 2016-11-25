(ns arcane-lab.api
  (:require [arcane-lab.bucket :as bucket]
            [arcane-lab.cards :as cards]
            [arcane-lab.utils :refer [rand-seed str->long]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as str]
            [compojure.core :refer [defroutes routes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [api-defaults wrap-defaults]]
            [ring.util.response :as resp]))

(defn pack-spec->codes
  "Turn a pack spec, which has the syntax of:

      pack-spec: [quantifier] set-code {pack-spec} ;
      quantifier: digit

  into a sequence of set code keywords. If quantifier is omitted, the
  default value of 1 is used. A set code is any valid three-letter set
  code for core, expansion, un, or masters sets. If a set code is unrecognized,
  there will be an appropriate number of nils in the returned sequence."
  [pack-spec]
  (loop [spec (seq pack-spec), codes ()]
    (if-let [maybe-quantifier (first spec)]
      (let [quantifier? (str->long maybe-quantifier)
            quantifier (or quantifier? 1)
            set-code (->> (if quantifier? (rest spec) spec)
                       (take 3)
                       (apply str)
                       str/upper-case
                       keyword)
            set-code (if (contains? cards/booster-sets set-code)
                       set-code)
            rest-spec (drop (if quantifier? 4 3) spec)]
        (recur rest-spec
               (concat codes (repeat quantifier set-code))))
      ;; else (done w/spec)
      codes)))

(defn unrecognized-sets
  [pack-spec]
  (let [upper-spec (str/upper-case pack-spec)
        codes (pack-spec->codes pack-spec)
        strip-code (fn [spec code]
                     (let [qtfr-and-code  (re-pattern (str "\\d?" (name code)))]
                       (str/replace spec qtfr-and-code "")))
        wout-recognized (reduce strip-code pack-spec (remove nil? codes))
        wout-qtfrs (str/replace wout-recognized #"\d" "")]
    (map (partial apply str) (partition 3 wout-qtfrs))))

(defn full-card->client-card
  [card]
  (-> card
    (rename-keys {:dfc? :dfc})
    (select-keys [:name :names :multiverseid :rarity :colors :manaCost :cmc :dfc :reverse])))

(defn- edn-resp
  ([thing] (edn-resp thing 200 {}))
  ([thing code] (edn-resp thing code {}))
  ([thing code headers]
   (let [headers (merge {"Content-Type" "application/edn"} headers)]
     {:status code
      :headers headers
      :body (pr-str thing)})))

(defn set-404
  [set-code]
  (edn-resp {:msg (str "Unknown set \"" set-code "\"")
             :kind "unknown-set"}
            404))

(defroutes booster-routes
  (GET "/booster/:set-code" [set-code]
       (let [set-code (-> set-code str/upper-case keyword)]
         (if (cards/booster-set-code? set-code)
           (edn-resp (cards/booster set-code))
           (set-404 set-code))))

  (GET "/pool/:pack-spec" [pack-spec]
       (let [seed (rand-seed)]
         (resp/redirect (str "/" pack-spec "/" seed))))

  (GET "/pool/:pack-spec/:seed" [pack-spec seed]
       (edn-resp (let [set-codes (pack-spec->codes pack-spec)
                       booster-count (count set-codes)]
                   (cond
                     (some nil? set-codes)
                     (let [msg (str "Could not recognize these set codes: "
                                    (str/join ", " (unrecognized-sets pack-spec)) ".")]
                       {:msg msg, :kind "unrecognized-set"} 400)

                     (not= booster-count 6)
                     (let [msg (str "A sealed pool requires exactly 6 booster packs, but"
                                    " you asked for " booster-count ".")]
                       {:msg msg, :kind "bad-booster-count"} 400)

                     :otherwise
                     (->> (cards/pool set-codes (str->long seed))
                       (map full-card->client-card)))))))

(defn set->api-metadata
  "Turn a set into its API representation, which is a subset of its metadata
  fields; the cards are not included, nor are fields related to intra-service
  compatibility like :magiccards_name or :mkm_name."
  [mtg-set]
  (-> mtg-set
    (select-keys [:name :code :releaseDate :border :type :booster])))

(defroutes set-routes
  (GET "/sets" [booster-only]
    (let [booster-filter? (not-any? #(= booster-only %)
                                    ["0" "false" "f" "n" nil])]
      (edn-resp (->> (vals cards/all-sets)
                (map set->api-metadata)
                (filterv (if booster-filter? :booster (constantly true))))
              200)))

  (GET "/sets/:set-code" [set-code]
    (if-let [mtg-set (cards/all-sets (keyword set-code))]
      (edn-resp (set->api-metadata mtg-set)
                200)
      (set-404 set-code))))

(defn decks-routes
  [decks-bucket]
  (GET "/decks/:deck-hash" [deck-hash]
       (if-let [card-names (bucket/bget decks-bucket deck-hash)]
         (edn-resp (->> card-names
                     (map (comp cards/oldest-printing cards/printings))
                     (map full-card->client-card)))
         {:status 404 :body "404: Deck not Found"})))

(def static-handlers
  (-> (routes booster-routes
              set-routes)
    (wrap-defaults api-defaults)))

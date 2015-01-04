(ns arcane-lab.api
  (:require [arcane-lab.cards :as cards]
            [arcane-lab.utils :refer [str->int]]
            [clojure.string :as str]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [api-defaults wrap-defaults]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]))

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
      (let [quantifier? (str->int maybe-quantifier)
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
  (select-keys card [:name :multiverseid :rarity :colors :manaCost :cmc]))

(defn- edn-resp
  ([thing] (edn-resp thing 200 {}))
  ([thing code] (edn-resp thing code {}))
  ([thing code headers]
   (let [headers (merge {"Content-Type" "application/edn"} headers)]
     {:status code
      :headers headers
      :body (pr-str thing)})))

(defroutes routes
  (GET "/booster/:set-code" [set-code]
       (if-let [booster (cards/booster (-> set-code str/upper-case keyword))]
         (edn-resp booster)
         (edn-resp {:msg (str "Unknown set \"" set-code "\"")
                     :kind "unknown-set"}
                    404)))

  (GET "/pool/:pack-spec" [pack-spec]
       (let [boosters (map cards/booster (pack-spec->codes pack-spec))
             cards (->> (apply concat boosters)
                     (map full-card->client-card))
             booster-count (count boosters)]
         (cond
           (some nil? boosters)
           (let [msg (str "Could not recognize these set codes: "
                          (str/join ", " (unrecognized-sets pack-spec)) ".")]
             (edn-resp {:msg msg, :kind "unrecognized-set"} 400))

           (not= booster-count 6)
           (let [msg (str "A sealed pool requires exactly 6 booster packs, but"
                          " you asked for " booster-count ".")]
             (edn-resp {:msg msg, :kind "bad-booster-count"} 400))

           :otherwise (edn-resp cards))))

  (route/not-found (pr-str {:msg "404 Not Found", :kind "not-found"})))

(def handler
  (-> routes
    (wrap-resource "")
    (wrap-content-type)
    (wrap-not-modified)
    (wrap-defaults api-defaults)))

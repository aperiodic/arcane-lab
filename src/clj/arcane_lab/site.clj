(ns arcane-lab.site
  (:require [arcane-lab.api :as api]
            [arcane-lab.bucket :as bucket]
            [arcane-lab.bucket.atom :as atom-bucket]
            [arcane-lab.bucket.file :as file-bucket]
            [arcane-lab.cards :as cards]
            [arcane-lab.http :as http]
            [arcane-lab.images :as images]
            [arcane-lab.import :as import]
            [arcane-lab.pages.import :as import-page]
            [arcane-lab.pages.pool :as pool-page]
            [arcane-lab.utils :refer [rand-seed sha1-str wrap-ignore-trailing-slash]]
            [clojure.string :as str]
            [compojure.core :refer [context routes GET POST]]
            [compojure.route :as route]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.defaults :refer [api-defaults site-defaults wrap-defaults]]
            [ring.middleware.not-modified :refer [not-modified-response
                                                  wrap-not-modified]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as resp]))

(defn cached-html-resp
  [resp req]
  (-> resp
    (not-modified-response req)
    (assoc-in [:headers "Content-Type"] "text/html")))

(defn html-resp
  [body]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body body})

(defonce images-cache (atom {}))

(defn site-routes
  ([] (site-routes (atom-bucket/init)))
  ([bucket]
   (routes
     (GET "/" [] (resp/redirect "/6DOM"))

     (GET "/decks/:deck-hash" req
          (cached-html-resp (resp/resource-response "/index.html") req))

     (GET "/import" []
          (html-resp (import-page/render nil nil)))

     (POST "/import" {:keys [form-params]}
           (let [input (get form-params "cards")
                 specs (->> (str/split input #"(\r\n|\r|\n)")
                         (map str/trim)
                         (remove empty?))
                 [cards-by-spec errors] ((juxt (partial remove import/spec-error?)
                                               (partial filter import/spec-error?))
                                         (map import/spec->cards specs))]
             (if (seq errors)
               (html-resp (import-page/render input errors))
               (let [card-names (map :name (apply concat cards-by-spec))
                     code (sha1-str (sort card-names))]
                 (bucket/bset bucket code card-names)
                 (resp/redirect (str "/decks/" code))))))

     (-> (context "/api" []
                  api/booster-routes
                  api/set-routes
                  (api/decks-routes bucket))
       (wrap-defaults api-defaults))

     (context "/img" []
              (images/image-routes images-cache))

     (GET "/:pack-spec" [pack-spec]
          (let [seed (rand-seed)]
            (resp/redirect (str "/" pack-spec "/" seed))))

     (GET "/:pack-spec/:seed" [pack-spec seed :as req]
       (if (= pack-spec "favicon.ico")
         (route/not-found "no favicon here")
         (let [seed (http/parse-int seed)
               pool (cards/pool-by-sheets (api/pack-spec->codes pack-spec) seed)]
            (-> (pool-page/render pool)
              (html-resp)
              (cached-html-resp req)))))

     (route/not-found "You are lost in the Maze of Ith. Go back!"))))

(defonce handler
  (-> (site-routes (file-bucket/init "/usr/share/arcane-lab"))
    (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
    (wrap-resource "")
    (wrap-content-type)
    (wrap-not-modified)
    (wrap-ignore-trailing-slash)))

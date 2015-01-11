(ns arcane-lab.site
  (:require [arcane-lab.api :as api]
            [arcane-lab.cards :as cards]
            [arcane-lab.utils :refer [rand-seed wrap-ignore-trailing-slash]]
            [compojure.core :refer [context defroutes GET]]
            [compojure.route :as route]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [ring.middleware.not-modified :refer [not-modified-response
                                                  wrap-not-modified]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as resp]))

(defroutes routes
  (GET "/" [] (resp/redirect "/6KTK"))

  (GET "/:pack-spec" [pack-spec]
       (let [seed (rand-seed)]
         (resp/redirect (str "/" pack-spec "/" seed))))

  (GET "/:pack-spec/:seed" req
       (-> (resp/resource-response "/index.html")
         (not-modified-response req)
         (assoc-in [:headers "Content-Type"] "text/html")))

  (context "/api" [] api/routes)

  (route/not-found "You are lost in the Maze of Ith. Go back!"))

(def handler
  (-> routes
    (wrap-defaults site-defaults)
    (wrap-resource "")
    (wrap-content-type)
    (wrap-not-modified)
    (wrap-ignore-trailing-slash)))

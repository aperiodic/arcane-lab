(ns arcane-lab.images
  (:import org.apache.http.cookie.MalformedCookieException)
  (:require [compojure.core :refer [routes GET]]
            [puppetlabs.http.client.sync :as http]
            [ring.middleware.not-modified :refer [wrap-not-modified]]))

(def source-ordering [:mtgdb-hi :mtgdb-lo :gatherer])

(comment (.indexOf source-ordering :mtgdb-lo))

(def sources
  (sorted-map-by #(< (.indexOf source-ordering %1) (.indexOf source-ordering %2))
    :mtgdb-hi (fn [m-id] (str "http://api.mtgdb.info/content/hi_res_card_images/" m-id ".jpg"))
    :mtgdb-lo (fn [m-id] (str "http://api.mtgdb.info/content/card_images/" m-id ".jpg"))
    :gatherer (fn [m-id] (str "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid="
                              m-id))))

(def acceptable-status? #{304 200})

(defn image-from-source
  [source multiverse-id]
  {:pre [(contains? sources source)]}
  (let [m-id->src (get sources source)]
    (http/get (m-id->src multiverse-id))))

(defn successful-gatherer-response?
  [resp]
  (or (not= (get-in resp [:content-type :mime-type]) "text/html")
      (not (re-find #"^http://gatherer\.wizards\.com" (get-in resp [:opts :url])))))

(defn image-and-source
  [multiverse-id]
  (loop [sources source-ordering]
    (if-let [source (first sources)]
      (let [{:keys [status] :as resp} (image-from-source source multiverse-id)]
        (if (and (acceptable-status? status) (successful-gatherer-response? resp))
          [source resp]
          (recur (next sources))))
      ;; else (no more sources left -- utter failure!)
      [nil {:status 404
            :body (str "Could not find image for multiverse-id")}])))

(comment

  (let [img (best-image 699)
        {{:strs [content-type last-modified]} :headers} img]
    (clojure.pprint/pprint (:headers img))
    )

  (-> (image-and-source 342564333333333)
    second
    )
  )

(defn source-memoized-best-image
  [an-atom]
  (fn [multiverse-id]
    (if-let [source (get @an-atom multiverse-id)]
      (http/get )
      (let [result (best-image multiverse-id)]
        ))))

(defn image-routes
  ([] (image-routes (atom {})))
  ([memoize ]
   (let [best-image (memoize best-image)]
    (-> (GET "/img/:m-id" [m-id]
             (let [{:keys [status headers body]} (best-image m-id)
                   {:strs [content-type content-length last-modified]} headers]
               (cond-> {:status status
                        :body body}
                 (acceptable-status? status)
                 (assoc :headers {:content-type content-type
                                  :content-length content-length
                                  :last-modified last-modified}))))
      (wrap-not-modified)))))

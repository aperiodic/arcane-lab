(ns arcane-lab.images
  (:import org.apache.http.cookie.MalformedCookieException)
  (:require [arcane-lab.utils :refer [now-rfc822]]
            [compojure.core :refer [routes GET]]
            [puppetlabs.http.client.sync :as http]
            [ring.middleware.not-modified :refer [wrap-not-modified]]))

(def sources [:mtgdb-hi :mtgdb-lo :gatherer])
(def startup-time (now-rfc822))

(def source->url-fn
  (sorted-map-by #(< (.indexOf sources %1) (.indexOf sources %2))
    :mtgdb-hi (fn [m-id] (str "http://api.mtgdb.info/content/hi_res_card_images/" m-id ".jpg"))
    :mtgdb-lo (fn [m-id] (str "http://api.mtgdb.info/content/card_images/" m-id ".jpg"))
    :gatherer (fn [m-id] (str "http://gatherer.wizards.com/Handlers/Image.ashx?type=card&multiverseid="
                              m-id))))

(def acceptable-status? #{304 200})

(defn get-card-image
  [source m-id req]
  {:pre [(contains? source->url-fn source)]}
  (if (get-in req [:headers "if-modified-since"])
    {:status 304, :headers {"content-type" "text/html"}} ;; always 304 if they seem to have it
    (http/get ((source->url-fn source) m-id))))

(defn successful-gatherer-response?
  [resp]
  (or (not= (get-in resp [:content-type :mime-type]) "text/html")
      (not (re-find #"^http://gatherer\.wizards\.com" (get-in resp [:opts :url])))))

(defn image-and-source
  [multiverse-id req]
  (loop [sources sources]
    (if-let [source (first sources)]
      (let [{:keys [status] :as resp} (get-card-image source multiverse-id req)]
        (if (and (acceptable-status? status) (successful-gatherer-response? resp))
          [source resp]
          (recur (next sources))))
      ;; else (no more sources left -- utter failure!)
      [nil {:status 404
            :body (str "Could not find image for multiverse-id")}])))

(defn source-memoized-best-image
  [!cache]
  (fn [multiverse-id req]
    (-> (if-let [source (get @!cache multiverse-id)]
          (get-card-image source multiverse-id req)
          (let [[source result] (image-and-source multiverse-id req)]
            (swap! !cache assoc multiverse-id source)
            result))
      (update-in [:headers "last-modified"] (fnil identity startup-time)))))

(defn image-routes
  ([] (image-routes (atom {})))
  ([!cache]
   (let [best-image (source-memoized-best-image !cache)]
    (GET "/:m-id" [m-id :as req]
             (let [{:keys [status headers body]} (best-image m-id req)
                   {:strs [content-type content-length last-modified]} headers]
               (cond-> {:status status
                        :body body}
                 (acceptable-status? status)
                 (update-in [:headers] (partial merge {"content-type" content-type
                                                       "content-length" content-length
                                                       "last-modified" last-modified}))))))))

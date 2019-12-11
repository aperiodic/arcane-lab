(ns arcane-lab.images
  (:import java.lang.Thread
           org.apache.http.cookie.MalformedCookieException)
  (:require [arcane-lab.external.scryfall :as scryfall]
            [arcane-lab.utils :refer [now-rfc822]]
            [clojure.java.io :as io]
            [compojure.core :refer [routes GET]]
            [puppetlabs.http.client.sync :as http]
            [ring.middleware.not-modified :refer [wrap-not-modified]]))


(def acceptable-status? #{304 200})
(def response-304 {:status 304, :headers {"content-type" "text/plain"}})

(def cacheable-headers
  ["age" "cache-control" "content-type" "content-length" "date" "etag"
   "last-modified"])

(defn content-length
  [resp]
  (Integer/parseInt (get-in resp [:headers "content-length"])))

(defn ->cacheable-resp
  [resp multiverse-id]
  (let [body-bytes (byte-array (content-length resp))]
    (.read (:body resp) body-bytes)
    {:status 200
     :headers (-> (:headers resp)
                (select-keys cacheable-headers))
     :body body-bytes}))

(defn- get-from-cache
  "When retrieving from the cache, the body is a byte array, which needs to be
  wrapped up in an InputStream before sending the response to ring."
  [!cache multiverse-id]
  (if-let [resp (get @!cache multiverse-id)]
    (update resp :body io/input-stream)))

(defn cached-image-resp
  "Find the image response for card with the `multiverse-id`, looking first in
  the `!cache` atom and requesting it from scryfall (and putting it in the
  `!cache`) on a miss."
  [!cache multiverse-id]
  (if-let [cached-resp (get-from-cache !cache multiverse-id)]
    cached-resp
    (let [img-resp (scryfall/card-img multiverse-id)
          cacheable-resp (->cacheable-resp img-resp multiverse-id)]
      (swap! !cache assoc multiverse-id cacheable-resp)
      (recur !cache multiverse-id))))

(defn memoized-card-image
  [!cache]
  (fn [multiverse-id req]
    (if (get-in req [:headers "if-modified-since"])
      response-304
      (try (cached-image-resp !cache multiverse-id)
        (catch java.net.UnknownHostException _
          {:status 404})))))

(defn image-routes
  ([] (image-routes (atom {})))
  ([!cache]
   (let [card-image (memoized-card-image !cache)]
    (GET "/:m-id" [m-id :as req]
      (card-image m-id req)))))

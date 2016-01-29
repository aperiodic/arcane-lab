(ns arcane-lab.utils
  (:require [bigml.sampling.simple]
            [clj-time.core :as time]
            [clj-time.format :as format-time]
            [clojure.string :as str])
  (:import java.security.MessageDigest
           java.util.Random))

(defn str->long
  [x]
  (let [str-x (if (char? x) (str x) x)]
    (try (Long/parseLong str-x)
      (catch NumberFormatException _
        nil))))

(defn words->key
  [words]
  (-> words
    str/lower-case
    (str/replace " " "-")
    keyword))

(defn sha1-str
  [thing]
  (let [md (MessageDigest/getInstance "SHA1")
        sha1-bytes (.digest md (.getBytes (str thing)))]
    (format "%x" (BigInteger. sha1-bytes))))

(defn rand-seed [] (+ (mod (System/currentTimeMillis) (* 365 24 60 60 1000))
                      (rand-int Integer/MAX_VALUE)))

(defn sample
  ([from seed]
   (bigml.sampling.simple/sample from :seed seed :generator :twister))
  ([from seed weights]
   (bigml.sampling.simple/sample from
                                 :weigh weights
                                 :seed seed
                                 :generator :twister)))

(defn seeded-rng
  [seed]
  (let [rng (Random. seed)]
    ;; if this is not done the initial value is similar for all seeds
    (dotimes [_ 1e2] (.nextLong rng))
    rng))

(defn now-rfc822
  []
  (let [rfc-formatter (format-time/formatters :rfc822)]
    (->> (time/now)
    (format-time/unparse rfc-formatter))))

(defn wrap-ignore-trailing-slash
  [handler]
  (fn [{:keys [uri] :as request}]
    (let [no-trailing (if (and (not= "/" uri) (.endsWith uri "/"))
                        (subs uri 0 (- (count uri) 1))
                        uri)]
      (handler (assoc request :uri no-trailing)))))

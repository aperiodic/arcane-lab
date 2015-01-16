(ns arcane-lab.utils
  (:require [bigml.sampling.simple]
            [clojure.string :as str]))

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

(defn wrap-ignore-trailing-slash
  [handler]
  (fn [{:keys [uri] :as request}]
    (let [no-trailing (if (and (not= "/" uri) (.endsWith uri "/"))
                        (subs uri 0 (- (count uri) 1))
                        uri)]
      (handler (assoc request :uri no-trailing)))))

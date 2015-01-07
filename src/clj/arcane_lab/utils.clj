(ns arcane-lab.utils
  (:require [bigml.sampling.simple]))

(defn str->int
  [x]
  (let [str-x (if (char? x) (str x) x)]
    (try (Integer/parseInt str-x)
      (catch NumberFormatException _
        nil))))

(defn sample
  ([from seed]
   (bigml.sampling.simple/sample from :seed seed :generator :twister))
  ([from seed weights]
   (bigml.sampling.simple/sample from
                                 :weigh weights
                                 :seed seed
                                 :generator :twister)))

(ns arcane-lab.useful
  #?(:clj  (:import [java.util UUID])
     :cljs (:require [cljs-uuid-utils.core :as uuid])))

(defn rand-uuid
  []
  #?(:clj  (UUID/randomUUID)
     :cljs (uuid/make-random-uuid)))

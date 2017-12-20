(ns dlp.useful.fn
  (:refer-clojure :exclude [any?]))

(defn any
  "Takes a list of predicates and returns a new predicate that returns true if any do.
  Copied from amalloy's useful; copied because useful is all clj (not cljc)."
  [& preds]
  (fn [& args]
    (some #(apply % args) preds)))

(defn any?
  [& preds]
  (comp
    boolean
    (apply any preds)))

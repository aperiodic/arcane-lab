(ns dlp.useful.map)

(defn merge-values-with
  "Create a new map by combining m's value at from-key with the value at to-key
  by calling f on the two values (the to-key value will be first), and make the
  result the new value of to-key. The from-key will be dissociated from the
  returned map."
  [m to-key from-key f]
  (-> m
    (update to-key f (get m from-key))
    (dissoc from-key)))

(defn merge-values
  "Create a new map by concatenating the collection in m's at from-key at the
  end of the one at to-key (so the new collection will have to-key's elements
  first). The from-key will be dissociated from the returned map."
  [m to-key from-key]
  (merge-values-with m to-key from-key concat))

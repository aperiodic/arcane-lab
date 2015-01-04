(ns arcane-lab.utils)

(defn str->int
  [x]
  (let [str-x (if (char? x) (str x) x)]
    (try (Integer/parseInt str-x)
      (catch NumberFormatException _
        nil))))

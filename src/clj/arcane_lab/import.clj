(ns arcane-lab.import
  (:require [arcane-lab.cards :as cards]))

(def spec-rgx #"(\d+)\S*\s+(.*)" )

(defn spec->cards
  [spec]
  (if-not (re-matches spec-rgx spec)
    {:error :spec, :value spec, :spec spec}
    (let [[_ amount-str nombre] (re-find spec-rgx spec)
          amount (try (Integer/parseInt amount-str) (catch NumberFormatException _ nil))
          card (first (cards/printings nombre))]
      (cond
        (nil? amount) {:error :amount, :value amount-str, :spec spec}
        (nil? card) {:error :cardname, :value nombre, :spec spec}
        :else (repeat amount card)))))

(defn spec-error?
  [x]
  (and (map? x) (:error x)))

(defn spec-error-message
  [spec-error]
  (let [{:keys [error spec value]} spec-error]
    (condp = error
      :amount (str "Could not recognize the number \"" value "\" in \"" spec "\".")
      :cardname (str "Could not find any cards named \"" value "\".")
      (str "The line \"" spec "\" must be a number (using digits) followed by a space"
           " and then a card name."))))

(ns arcane-lab.history
  (:require [arcane-lab.constants :as c]
            [cljs.reader :as reader]
            [clojure.string :as str]))

;;
;; Saving & Loading State
;;

(defn state-key
  [pack-spec seed]
  (str pack-spec "|" seed))

(defn path-components
  [page-path]
  (-> page-path
    (str/replace #"^/" "")
    (str/split #"/")))

(defn save-state!
  [state]
  (let [page-path (-> js/document .-location .-pathname)
        [ps seed] (path-components page-path)
        pool-key (state-key ps seed)
        ts (-> (js/Date.) .getTime (/ 1000) int str)
        clean-state (-> state
                      (dissoc :drag)
                      (dissoc :selection))
        serialized (str ts (pr-str clean-state))]
    (try
      (.setItem js/localStorage pool-key serialized)
      (catch js/Error e
        (if (= (.-name e) "QuotaExceededError")
          (do
            (.cleanStorage js/window)
            (.setItem js/localStorage pool-key serialized))
          (throw e))))))

(defn load-state
  [pack-spec seed]
  (if-let [saved-state (.getItem js/localStorage (state-key pack-spec seed))]
    (let [unsorted-state (reader/read-string
                           (.substr saved-state c/ts-digits))]
      {:piles (into (sorted-map) (for [[y row] (:piles unsorted-state)]
                                   [y (into (sorted-map) (for [[x pile] row]
                                                           [x pile]))]))})))

;;
;; State Manipulation
;;

(def !fate (atom {:past [], :future ()}))

(defn add-new-state!
  [state]
  (swap! !fate (fn [{:keys [past] :as fate}]
                 (if (= state (last past))
                   fate
                   (-> fate
                     (update-in [:past] (fnil conj []) state)
                     (assoc :future ())))))
  state)

(defn zip-to-past
  [fate]
  (let [{:keys [past]} fate]
    (if (<= (count past) 1)
      fate
      (-> fate
        (update-in [:past] (comp vec butlast))
        (update-in [:future] (fnil conj ()) (last past))))))

(defn rewind!
  [current]
  (let [fate' (swap! !fate zip-to-past)]
    (-> fate' :past last)))

(defn zip-to-future
  [fate]
  (if-let [new-present (-> fate :future first)]
    (-> fate
      (update-in [:future] rest)
      (update-in [:past] (fnil conj []) new-present))
    fate))

(defn fast-forward!
  [current]
  (let [fate' (swap! !fate zip-to-future)]
    (-> fate' :past last)))

(ns arcane-lab.pages.import
  (:require [arcane-lab.import :as import]
            [clojure.string :as str]
            [hiccup.page :refer [html5 include-css]]))

(defn render
  ([] (render nil nil))
  ([cards errors]
   (html5
     [:head
      [:meta {:charset "utf-8"}]
      [:title "Lab Maniac: Import"]
      (include-css "/lab.css")
      [:meta
       {:name "description"
        :content "A webapp that lets you practice building Magic: the Gathering sealed pools"}]]

     [:body
      [:div#wrapper
       [:form {:action "import", :method "POST"}
        [:h2 "Cards"]
        (when (seq errors)
          [:p.error (str/join "<br>" (map import/spec-error-message errors))])
        [:p
         [:textarea {:name "cards", :cols 100, :rows 35}
          (str cards)]]
        [:input {:type "submit", :value "Import"}]]]])))

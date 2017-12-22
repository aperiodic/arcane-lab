(ns arcane-lab.pages.pool
  (:require [arcane-lab.ui :as ui]
            [hiccup.page :as hic]))

(defn translation
  [x y]
  (str "transform: translate(" (int x) "px," (int y) "px);"))

(defn card
  [{:keys [x y name img-src]}]
  [:div.card {:style (translation x y)}
   [:img {:src img-src :title name}]])

(defn pile
  [the-pile]
  [:div.pile
   (map card (:cards the-pile))])

(declare head)
(declare help-widget)
(declare gh-ribbon)
(declare ui-bar)

(defn render
  "Given a structured sealed pool, return the page with that pool in the initial
  UI state as an HTML string"
  [cards-by-sheet]
  (let [piles (ui/pool->piles cards-by-sheet)]
    (hic/html5
      head
      [:body
       help-widget
       gh-ribbon
       ui-bar

       [:div#app
        [:div#dom-root
         (map pile piles)
         [:div#loader
          [:p
           [:img {:src "/loading.svg"}]]]]]

       [:script {:src "/js/static/react-0.11.1.js"}]
       [:script {:src "/js/static/cleanStorage.js"}]
       [:script {:src "/js/compiled/goog/base.js"}]
       [:script {:src "/js/compiled/arcane-lab.js"}]
       [:script {:type "text/javascript"} "goog.require(\"arcane_lab.main\");"]])))

(def head
  [:head
   [:meta {:charset "utf-8"}]
   [:title "Lab Maniac"]
   [:meta {:name "description"
           :content "A webapp that lets you practice building Magic: the Gathering sealed pools"}]
   (hic/include-css "/lab.css")])

(def help-widget
  [:div#meta {:style "display: none"}
   [:p "Lab Maniac lets planeswalkers practice their craft. Specifically, building sealed pools. "]
   [:p
    "Move cards by clicking on them and dragging with your mouse.
    You can also move multiple cards by selecting them first.
    To select cards, click and drag on the background between the cards and then highlight the cards you want selected.
    Once cards have been selected, click on them and drag to move all the selected cards to a new place."]
   [:p
    "Use the \"Undo\" and \"Redo\" buttons to undo and redo the card movements you've made.
    You can also use the \"u\" and \"r\" keys on your keyboard as shortcuts for undo &amp; redo."]
   [:p
    "The \"New Pool\" button will generate a new pool in the same format as the current pool.
    You can change the format by using the \"Change Format\" selection box.
    Once you select a new format, a new pool in that format will be generated.
    You can also make custom formats by editing the URL directly; the format is a number of packs followed by the set code for those packs, and multiple sets can be used.
    For example, '3EMN3SOI' makes a pool with three Eldritch Moon packs and three Shadows over Innistrad packs."]
   [:p
    "The way that you've layed out your pool will be saved in your browser.
    This means that if you lay out a deck in a pool, you can go back to the pool later and the deck will still be there.
    But, since the piles are just saved in your browser, if you use a different browser, or send the link to somebody else, they'll only see the default starting layout.
    There is limited space available to save the piles; once you get to around 100 saved pools, the oldest pools will be deleted to free up space for the new ones."]])

(def gh-ribbon
  [:div
   [:img {:id "gh-ribbon"
          :style "position: absolute; top: 0; right: 0; border: 0;"
          :src "https://camo.githubusercontent.com/52760788cde945287fbb584134c4cbc2bc36f904/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f77686974655f6666666666662e706e67"
          :data-canonical-src "https://s3.amazonaws.com/github/ribbons/forkme_right_white_ffffff.png"
          :alt "Fork me on GitHub"
          :usemap "#gh-ribbon-map"
          }]
   [:map {:id "gh-ribbon-map"
          :name "gh-ribbon-map"}
    [:area {:alt "Fork me on GitHub"
            :title "Fork me on GitHub"
            :href "https://github.com/aperiodic/arcane-lab"
            :target "_blank"
            :shape "poly"
            :coords "16,0,149,134,149,91,58,0"}]]])

(def ui-bar
  [:div#ui
   [:span.button [:a#help-button {:href "#"} [:button "?"]]]
   [:span.button [:a#undo-button {:href "#"}
                  [:button [:span.underline "U"] "ndo"]] ]
   [:span.button [:a#redo-button {:href "#"}]
    [:button [:span.underline "R"] "edo"]]
   [:span.button [:a#new-pool-button {:href "./"} [:button "New Pool"]] ]
   [:span#navigator
    "Change Format: "
    [:select {:name "set" :default-value "blank"}
     [:option {:value "blank"} "&nbsp;"]
     [:option "Champions of Kamigawa"]]] ; currently longest setname, change if new set beats it
   [:span.button [:a#import-button {:href "../../import"}
                  [:button "Import Decklist"]]]])

(ns arcane-lab.render
  (:require [arcane-lab.constants :as c]
            [arcane-lab.piles :as piles]
            [arcane-lab.sets :as sets]
            [arcane-lab.state :as state]
            [om.dom :as dom :include-macros true]   ))

(defn- css-translation
  [x y]
  (str "translate(" x "px," y "px)"))

(defn selection
  [state]
  (if-let [selection (:selection state)]
    (let [selected-count (->> (state/state->cards state)
                           (filter :selected?)
                           count)
          [ox oy] (:stop selection)
          [left right top bottom] (piles/selection-edges selection)]

      (dom/div nil
               (dom/div #js {:id "selection"
                             :className "box"
                             :style #js {:transform (css-translation left top)
                                         :width (- right left)
                                         :height (- bottom top)}})

               (if (pos? selected-count)
                 (let [cx (- ox (* 4 c/em))
                       cy (- oy (* 1.25 c/em))]
                   (dom/div #js {:id "counter"
                                 :className "badge"
                                 :style #js {:transform (css-translation
                                                          (int cx) (int cy))}}
                          (str selected-count))))))))

(defn card
  ([card] (card card 0 0))
  ([card dx dy]
   (let [{:keys [name id img-src x y selected? dropped?]} card]
     (dom/div #js {:className (if selected? "card selected" "card")
                   :style #js {:transform (css-translation (+ x dx) (+ y dy))}
                   :key id}
              (dom/img #js {:src img-src, :title name})
              (if dropped?
                (dom/div #js {:className "dropped-marker"}))))))

(defn pile
  [pile]
  (dom/div #js {:className "pile"}
           (map #(card % 0 0) (:cards pile))))

(defn drag
  [state]
  (if-let [drag (:drag state)]
    (let [[tx ty insertion-index] (:drag-target state)
          target-pile (state/get-pile state tx ty)
          target-height (if target-pile
                          (piles/pile-height target-pile)
                          c/card-height)]
      (dom/div nil
               (dom/div #js {:id "drag-target"
                             :className "ghost"
                             :style #js {:transform (css-translation tx ty)
                                         :height target-height}})
               (if (number? insertion-index)
                 (let [margin 6
                       bar-x (+ tx margin)
                       bar-y (+ ty (* insertion-index c/pile-stride) 4)]
                   (dom/div #js {:id "drop-target"
                                 :className "overlay"
                                 :style #js {:transform (css-translation
                                                          bar-x bar-y)}})))
               (apply dom/div #js {:id "drag" :className "pile"}
                      (map #(card % 0 0) (:cards drag)))))))

(defn dfc
  [state]
  (if (get-in state [:drag :dfcs?])
    (let [{dx :x dy :y} (:drag state)]
      (apply dom/div #js {:className "dfc-back-container"
                          :style #js {:transform (css-translation
                                                   (+ dx c/card-width) dy)}}
             (map-indexed (fn [i c]
                            (if c
                              (card c 0 (* i c/pile-stride))))
                          (map :reverse (get-in state [:drag :cards])))))))

(defn dfc-preloader
  [state]
  (if-let [dfcs (seq (:dfcs state))]
    (dom/div #js {:className "dfc-preloader"}
             (for [{:keys [img-src id]} (map :reverse dfcs)]
               (dom/img #js {:src img-src
                             :key id
                             :style #js {:display "block"}})))))

(defn hud
  [state]
  (let [trimmed-state (-> (if (contains? state :drag)
                            (update-in state, [:drag :cards]
                                       (partial map (comp str :id)))
                            state)
                        (update :dfcs (partial map (comp str :id)))
                        (dissoc :piles :selection-triggers))]
    (dom/div #js {:id "hud"}
             (dom/pre nil
                      (dom/b nil
                             (.stringify js/JSON
                               (clj->js trimmed-state) nil 2))))))

(defn drop-zone-lines
  [state]
  (let [{xs :vertical, ys :horizontal} (:drag-triggers state)]
    (dom/div nil
             (concat
               (for [x xs]
                 (dom/div #js {:className "debug line vertical"
                               :style #js {:left x}}))
               (for [y ys]
                 (dom/div #js {:className "debug line horizontal"
                               :style #js {:top y}}))))))

(defn footer
  [state]
  (let [{:keys [drag piles]} state
        max-y (+ (apply max (keys piles))
                 (piles/row-height (get piles (last (keys piles)))))] ;; TODO replace w/last-row fn
    (dom/div #js {:id "footer"
                  :style #js {:transform (css-translation 0 max-y)}}
             (dom/div #js {:className "disclaimer"}
                      (dom/strong nil "Magic: the Gathering")
                      " is © Wizards of the Coast"
                      " • Lab Maniac is in no way affiliated with Wizards of the Coast")
             (dom/div #js {:className "disclaimer"}
                      "Data from " (dom/a #js {:href "http://mtgjson.com"} "mtgjson.com")
                      ", images from " (dom/a #js {:href "http://magiccards.info"} "magiccards.info & Gatherer")
                      " • Made by Dan Lidral-Porter"))))

(defn cards
  [state !loaded?]
  (dom/div #js {:id "dom-root"}
           (if-not @!loaded?
             (dom/div #js {:id "loader"} (dom/img #js {:src "/egg.gif"})))
           (apply dom/div {:id "piles"}
                  (map pile (state/state->piles state)))
           (dfc state)
           (drag state)
           (selection state)
           (footer state)
           (dfc-preloader state)))

(defn- navigate!
  "Redirect to the given sealed format."
  [format-str]
  (set! (.-location js/document) (str "/" format-str)))

(defn navigator
  ([all-sets] (navigator all-sets nil))
  ([all-sets current-format]
   (let [valid-sets (filter (comp sets/sets-that-work keyword :code) all-sets)
         known-formats (into #{} (map :sealed-format valid-sets))
         custom-format? (not (contains? known-formats current-format))
         options (if-not custom-format?
                   valid-sets
                   (conj valid-sets
                         {:code "custom"
                          :name (str "Custom - " current-format)
                          :release-date "9999-99-99"}))]
     (dom/span #js {}
       "Change Format: "
       (dom/select #js {:className "om-selector"
                        :name "set"
                        :defaultValue (or current-format "6KLD")
                        :onChange (fn [event]
                                    (navigate! (-> event .-target .-value)))}
                   (for [{:as mtg-set
                          :keys [code sealed-format]} (->> options
                                                        (sort-by :release-date)
                                                        reverse)
                         :let [id (str "select-set-option-" code)]]
                     (dom/option #js {:value sealed-format
                                      :id id, :key id}
                                 (:name mtg-set))))))))

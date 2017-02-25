(ns arcane-lab.constants)

;;
;; Constants
;;

(def card-width 222)
(def card-height 319)

(def em 18)

(def half-card-width (int (/ card-width 2)))
(def half-card-height (int (/ card-height 2)))

(def gutter (int (/ card-width 8)))
(def half-gutter (int (/ gutter 2)))
(def pile-stride (int (/ card-height 9.5)))
(def pile-spacing (+ card-width gutter))

(def drag-x-offset (/ card-width 2))
(def drag-y-offset (-> (* 0.4 card-height) int))

(def mouse-y-offset 48)

(def u-key-code 85)
(def r-key-code 82)

;; not technically constant, but will be for my lifetime
(def ts-digits (-> (js/Date.) .getTime (/ 1000) int str count))


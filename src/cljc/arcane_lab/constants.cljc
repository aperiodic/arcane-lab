(ns arcane-lab.constants
  (:require [dlp.useful.math :refer [half]]))

;;
;; Constants
;;

(def card-width 244)
(def card-height 351)

(def em 20)

(def half-card-width (int (half card-width)))
(def half-card-height (int (half card-height)))

(def gutter (int (/ card-width 8)))
(def half-gutter (int (half gutter)))
(def pile-stride (int (/ card-height 9.5)))
(def pile-spacing (+ card-width gutter))

(def drag-x-offset (half card-width))
(def drag-y-offset (int (* 0.4 card-height)))

(def mouse-y-offset 48)

(def u-key-code 85)
(def r-key-code 82)

;; Number of base 10 digits in a Unix timestamp. Not technically constant, but
;; will be for my lifetime
(def ts-digits 10)

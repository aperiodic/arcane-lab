(ns arcane-lab.external.scryfall
  (:require [puppetlabs.http.client.sync :as http]))

(def scryfall-base "https://api.scryfall.com")

(def card-by-m-id-route "/cards/multiverse/")

(defn card-img
  [multiverse-id]
  (http/get
    (str scryfall-base card-by-m-id-route multiverse-id)
    {:query-params {"format" "image"}}))

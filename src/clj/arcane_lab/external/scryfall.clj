(ns arcane-lab.external.scryfall
  (:require [arcane-lab.schedule :as sched]
            [puppetlabs.http.client.sync :as http]))

(def scryfall-base "https://api.scryfall.com")
(def card-by-m-id-route "/cards/multiverse/")

(def request-period-ms 100)

(def ^:private !scryfall-throttle (sched/new-schedule))

(defn card-img
  [multiverse-id]
  (let [;; find the next time we can make a request to scryfall without
        ;; exceeding their requested rate limit
        request-time (sched/schedule-open-slot! !scryfall-throttle
                                                request-period-ms)
        now (System/currentTimeMillis)
        ;; sleep until we can make that request
        _ (when (< now request-time)
            (Thread/sleep (- request-time now)))]
    (http/get
      (str scryfall-base card-by-m-id-route multiverse-id)
      {:query-params {"format" "image"}})))

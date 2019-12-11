(ns arcane-lab.schedule)

(defn- thread-id
  []
  (.getId (Thread/currentThread)))

(defn- curr-ms []
  (System/currentTimeMillis))

(defn new-schedule []
  (atom []))

(defn- schedule-slot
  [schedule period my-id]
  (let [now (curr-ms)]
    (if (empty? schedule)
      [{:time now, :thread my-id}]
      (let [{last-slot :time} (last schedule)
            current-slots (drop-while #(< (:time %) (- now period)) schedule)]
        (if (empty? current-slots)
          [{:time now, :thread my-id}]
          (conj (vec current-slots) {:time (+ last-slot period)
                                     :thread my-id}))))))

(defn schedule-open-slot!
  [!schedule period]
  (let [my-id (thread-id)
        new-sched (swap! !schedule schedule-slot period my-id)
        {slot-time :time} (-> (drop-while #(not= (:thread %) my-id) new-sched)
                            first)]
    slot-time))

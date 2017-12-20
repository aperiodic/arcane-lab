(ns dlp.useful.math)

(defn half
  [x]
  (/ x 2))

(defn mean
  [x y]
  (-> (+ x y) (/ 2)))

(defn mean-int
  [x y]
  (-> (+ x y) (/ 2) int))

(defn binary-search
  "Given a sorted collection and a number, return the index at which the number
  would be inserted in the collection."
  ([xs x] (let [n (count xs)]
            (cond
              (<= n 0) 0
              (= n 1) (if (<= x (nth xs 0)) 0 1)
              :else (binary-search xs x 0 n))))
  ([xs x i_min i_max]
   (if (= i_min i_max)
     i_min
     (let [i_mean (mean-int i_min i_max)
           comparison (compare x (nth xs i_mean))]
       (cond
         (neg? comparison) (if (= i_min i_mean)
                             i_min
                             (recur xs x i_min i_mean))
         (pos? comparison) (if (= i_min i_mean)
                             (recur xs x (inc i_mean) i_max)
                             (recur xs x i_mean i_max))
         (zero? comparison) i_mean)))))

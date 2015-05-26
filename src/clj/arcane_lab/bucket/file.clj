(ns arcane-lab.bucket.file
  (:require [arcane-lab.bucket :as bucket])
  (:import java.io.File))

(defrecord FileBucket
  [root]

  bucket/Bucket
  (bget [_ code]
    (let [file (File. (str root "/" code))]
      (if (.exists file)
        (read-string (slurp file)))))
  (bset [_ code value]
    (spit (str root "/" code) (pr-str value))
    value))

(defn init
  [dir]
  (FileBucket. dir))

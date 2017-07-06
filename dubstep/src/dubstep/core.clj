(ns dubstep.core
  (:require [clojure.string :as s]))

(def wrapping-regex #"^(WUB)*(.*?)(WUB)*$")
(def mid-regex #"(.*?)(WUB)+")

(defn song-decoder
  [s]
  (when-let [mid-s (->> s 
                        (re-matches wrapping-regex)
                        (rest)
                        (rest)
                        (first))]
    ;; Add a single copy of "WUB" to the end so matching is easier.
    (let [s (re-seq mid-regex (str mid-s "WUB"))]
      (s/join " " (map second s)))))

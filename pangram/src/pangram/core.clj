(ns pangram.core
  (require [clojure.string :refer [lower-case]]))

(def alphabet
  (set (map char (range (int \a)
                        (inc (int \z))))))

(defn pangram?
  [s]
  (let [filtered-s (filter (partial contains? alphabet) (lower-case s))]
    (= (set filtered-s) alphabet)))

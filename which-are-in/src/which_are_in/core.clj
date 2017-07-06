(ns which-are-in.core)

(defn includes?
  [s substr]
  "Returns true if s includes substr. Added in Clojure 1.8."
  (.contains (.toString s) substr))

(defn in-array
  [array1 array2]
  (let [substr? (fn [s] (some #(includes? % s) array2))]
    (vec (sort (filter substr? array1))) ))

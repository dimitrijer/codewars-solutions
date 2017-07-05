(ns arge.core)

(defn nb-year
  [p0 percent aug p]
  (let [percent-incr (+ (/ percent 100.0) 1)
        iter-p (fn [starting-pop] (+ aug (int (* percent-incr starting-pop))))]
    (count (take-while #(> p %) (iterate iter-p p0)))))

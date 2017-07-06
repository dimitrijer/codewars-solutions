(ns bouncing-balls.core)

(defn bouncing-balls-impl
  [h bounce window]
  {:pre [(pos? h)
         (< window h)
         (and (> bounce 0) (< bounce 1))]}
  (let [height-seq (iterate #(* % bounce) h)
        bounces-fn (fn [height]
                     (if (> height window)
                       (if (> (* height bounce) window) 2 1)))]
    (reduce + (take-while some? (map bounces-fn height-seq)))))

(defn bouncing-balls
  [h bounce window]
  (try 
    (bouncing-balls-impl h bounce window)
    (catch AssertionError e -1)))

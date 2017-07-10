(ns leading-zeros.core)

(defn pow [x n] (apply * (take n (repeat x))))

(defn factors-of-five
  "A lazy sequence of quotients of x with increasing factors of 5: 5, 25, 125,
  etc. Sum of this sequence equals number of trailing zeros in factorial of x."
  ([x] (factors-of-five x 1))
  ([x n]
   (let [quotient (quot x (pow 5 n))]
     (lazy-seq (cons quotient (factors-of-five x (inc n)))))))

(defn zeros
  [n]
  (apply + (take-while pos? (factors-of-five n))))

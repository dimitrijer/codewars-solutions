(ns com-denom.core)

(defn gcd [a b]
  (let [lesser (min a b)
        greater (max a b)
        r (rem greater lesser)]
    (if (= r 0)
      lesser
      (recur lesser r))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn reformat-frac [lcm frac]
  [(* (first frac) (/ lcm (second frac))), lcm])

(defn convert-fracts [lst]
  (let [denoms (map second lst)
        lcm (reduce lcm denoms)]
    (vec (map (partial reformat-frac lcm) lst))))

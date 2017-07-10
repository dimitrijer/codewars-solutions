(ns stringmerge.core)

(defn is-merge
  [s p1 p2]
  (if (= (count s) (+ (count p1) (count p2)))
    (if-not (empty? s)
      (or
        (if (= (first s) (first p1)) (is-merge (rest s) (rest p1) p2))
        (if (= (first s) (first p2)) (is-merge (rest s) p1 (rest p2)))
        ;; No way to go.
        false)
      ;; Exhausted initial string.
      true)
      ;; Lengths do not match.
      false))

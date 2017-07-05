(ns backwards-prime.core
  (require [clojure.set :refer [difference]]
           [clojure.string :as s]))

(defn multiples
  "Returns a lazy seq of multiples of n * i, starting from i = 3 by default and
  incrementing in steps of 2, so i = 3, i = 5, i = 7 etc."
  ([n i]
   (lazy-seq (cons (* n i) (multiples n (+ i 2)))))
  ([n] (multiples n 3)))

(defn primes
  "Produces a set of primes up to (and including) stop."
  [stop]
  (let [candidates (apply sorted-set (filter odd? (range 3 (inc stop))))
        sieve-primes (fn [candidates n]
                       (let [largest-candidate (first (rseq candidates))]
                         (if (or (not (contains? candidates n))
                                 (> (* n 3) largest-candidate))
                           candidates
                           (difference candidates
                                       (set (take-while (partial >= largest-candidate)
                                                        (multiples n)))))))]
    (reduce sieve-primes candidates candidates)))

(defn backwards?
  "Checks if there is a backwards prime in ps corresponding to prime p."
  [ps p]
  (let [s (str p)
        backward-p (Integer/parseInt (s/reverse s))]
    (and (> (count s) 1)              ;; only two, or more, digit numbers
         (not= p backward-p)          ;; palindromes don't count
         (contains? ps backward-p)))) ;; has to be a prime as well

(defn backwards-prime
  [start stop]
  (let [stop-max (apply max (map #(-> % str s/reverse Integer/parseInt)
                                 (range start (inc stop))))
        ps (primes (max stop stop-max))
        backward-ps (filter (partial backwards? ps) ps)]
    (vec (sort (filter #(and (<= start %)
                             (>= stop %)) backward-ps)))))

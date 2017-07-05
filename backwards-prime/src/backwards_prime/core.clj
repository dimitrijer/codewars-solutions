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

(defn any?
  [p coll]
  (if-not (empty? coll)
    (if (p (first coll))
      true
      (recur p (rest coll)))
    false))

(def cached-primes (primes 100000))

(defn looks-prime?
  "If this returns true, n is likely to be a prime."
  [n]
  (if (odd? n)
    (if (contains? cached-primes n)
      true
      (not (any? #(= (mod n %) 0) cached-primes)))
    false))

(defn backwards?
  "Checks if there is a backwards prime in ps corresponding to prime p."
  [ps p]
  (let [backward-p (-> p str s/reverse Integer/parseInt)]
    (and (not= p backward-p)          ;; palindromes don't count
         (looks-prime? backward-p)))) ;; has to be a prime as well

(defn backwards-prime
  [start stop]
  (let [prime-range (range start (inc stop))
        ps (set (filter looks-prime? prime-range))
        result (filter (partial backwards? ps) ps)]
    (vec (sort result))))

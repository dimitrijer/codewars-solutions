(ns caesar.core)

(defn char-range
  [from to]
  (map char (range (int from) (inc (int to)))))

(def upper-alphabet (vec (char-range \A \Z)))
(def lower-alphabet (vec (char-range \a \z)))
(def supported-chars (into #{} (concat upper-alphabet lower-alphabet)))

(defn rotate
  "Rotates a coll right specified number of times. If times is negative, left
  rotation is performed.

  Not really efficient."
  ([coll]
   (conj (vec (rest coll)) (first coll)))
  ([coll times]
   (let [i (if (pos? times)
             times
             (mod times (count coll)))]
     (nth (iterate rotate coll) i))))

(defn shift-char
  "Shifts a character for shift places in its alphabet."
  [c shift]
  (if (contains? supported-chars c)
    (let [alphabet (if (Character/isUpperCase c)
                     upper-alphabet
                     lower-alphabet)
          index (.indexOf alphabet c)
          shifted-alphabet (rotate alphabet shift)]
      (get shifted-alphabet index))
    ;; Don't shift characters that are not supported.
    c))

(defn encrypt
  ([s shift] (encrypt s shift identity))
  ([s shift shift-fn]
   (let [shifts (map shift-fn (range shift (+ shift (count s))))]
     (apply str (map shift-char s shifts)))))

(defn decrypt [e shift] (encrypt e shift -))

(defn part-size [len]
  "Figures out part size. If it's not possible to create 5 equal parts, size is
  determined so that fifth, incomplete part is as long as possible."
  (let [size (quot len 5)
        remainder (mod len 5)]
    (if (= remainder 0)
      ;; Five equal parts.
      size
      ;; Fifth part incomplete. Maximize its size.
      (loop [i size]
        (let [incomplete-size (- len (* 4 i))]
          (if (< incomplete-size size)
            (recur (dec i))
            (inc i)))))))

(defn moving-shift
  [s shift]
  (let [e (encrypt s shift)
        size (part-size (count s))
        parts (vec (partition size size [] e))]
    (vec (map #(apply str %) (if (< (count parts) 5)
                               (conj parts '())
                               parts)))))

(defn demoving-shift [es shift] (decrypt (apply str es) shift))

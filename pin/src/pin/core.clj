(ns pin.core)

(def digits
  [[1 2 3]
   [4 5 6]
   [7 8 9]
   [nil 0 nil]])

(defn possible-digits
  ([[x y]] (possible-digits x y))
  ([x y]
   (filter some? (map #(get-in digits %) [[x y]
                                          [(dec x) y] [(inc x) y]
                                          [x (dec y)] [x (inc y)]]))))

(defn index-of
  [digit]
  (first (for [[x row] (map-indexed vector digits)
               [y value] (map-indexed vector row)
               :when (= digit value)]
           [x y])))

(defn str->ints
  [digits-str]
  (let [converter #(Integer/parseInt %)]
    (into [] (map (comp converter str) digits-str))))

(defn get-pins
  [digits]
  (if (string? digits)
    (get-pins (str->ints digits))
    (let [permutations (map (comp possible-digits index-of) digits)
          reducer (fn [suffixes prefixes]
                    (for [prefix prefixes
                          suffix suffixes]
                      (str prefix suffix)))]
      (reduce reducer [""] (reverse permutations)))))

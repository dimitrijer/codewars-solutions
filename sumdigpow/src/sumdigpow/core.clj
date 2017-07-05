(ns sumdigpow.core)

(defn num->eureka
  [n]
  (let [digits (map #(Integer/parseInt (str %)) (str n))
        reducer-fn (fn [{:keys [exp sum]} digit]
                     {:sum (+ sum (int (Math/pow digit exp)))
                      :exp (inc exp)})
        result (:sum (reduce reducer-fn {:exp 1 :sum 0} digits))]
    (when (= n result) result)))

(defn sum-dig-pow
  [a b]
  (vec (sort (filter (complement nil?) (map num->eureka (range a (inc b)))))))

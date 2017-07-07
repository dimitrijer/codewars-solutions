(ns human-time.core)

(defn human-readable
  [x]
  (let [hours (int (/ x 3600))
        mins (int (/ (rem x 3600) 60))
        secs (rem (rem x 3600) 60)]
    (format "%02d:%02d:%02d" hours mins secs)))

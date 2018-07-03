(ns simple-pig-latin.core
  (require [clojure.string :as str]))

(defn ->words [s] (str/split s #" "))
(def punctuation? #{\! \. \? \,})

(defn permute-word [word]
  (let [first-char (first word)]
    (if-not (punctuation? first-char)
      (str (apply str (rest word)) first-char "ay")
      word)))

(defn ->pig-latin [text] (str/join " " (map permute-word (->words text))))

(ns day08.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn count-regex-matches [total seq-matches]
  (+ total (->> seq-matches
                (filter (comp not nil?))
                count
                dec)))

(defn count-decoded-char-difference [s]
  (reduce count-regex-matches
          0
          (re-seq #"(\"|\\\\|\\\"|(\\x)(..))" s)))

(defn count-encoded-char-difference [s]
  (reduce count-regex-matches
          2
          (re-seq #"(\"|\\|(\\x)(..))" s)))

(defn -main [part]
  (let [input (slurp "input.txt")
        lines (str/split-lines input)]
    (println (case (str part)
               "1" (reduce + (map count-decoded-char-difference lines))
               "2" (reduce + (map count-encoded-char-difference lines))
               (throw (Exception. "`part` must be 1 or 2"))))))

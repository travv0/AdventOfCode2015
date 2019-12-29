(ns day08.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn count-char-difference [s]
  (reduce #(+ %1 (->> %2
                      (filter (comp not nil?))
                      count
                      dec))
          0
          (re-seq #"(\"|\\\\|\\\"|(\\x)(..))" s)))

(defn -main [part]
  (let [input (slurp "input.txt")
        lines (str/split-lines input)]
    (case (str part)
      "1" (reduce + (map count-char-difference lines))
      "2" (throw (Exception. "unimplemented"))
      (throw (Exception. "`part` must be 1 or 2")))))

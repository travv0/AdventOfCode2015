(ns day05.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn n-vowels? [s n]
  (>= (count (filter #{\a \e \i \o \u} s))
      n))

(defn repeating-letter? [s]
  (loop [head (first s)
         tail (rest s)]
    (and (not (nil? head))
         (or (= head (first tail))
             (recur (first tail) (rest tail))))))

(defn excludes-substrings? [s substrings]
  (not-any? (partial str/includes? s) substrings))

(defn nice? [s]
  (and (n-vowels? s 3)
       (repeating-letter? s)
       (excludes-substrings? s ["ab" "cd" "pq" "xy"])))

(defn -main []
  (let [strings (str/split-lines (slurp "input.txt"))]
    (count (filter nice? strings))))

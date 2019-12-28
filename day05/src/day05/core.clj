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

(defn repeating-letters? [s]
  (loop [head (first s)
         tail (rest s)]
    (and (not (nil? head))
         (or (str/includes? (str/join (rest tail)) (str head (first tail)))
             (recur (first tail) (rest tail))))))

(defn repeat-with-char-between? [s]
  (loop [head (first s)
         tail (rest s)]
    (and (not (nil? head))
         (or (= head (second tail))
             (recur (first tail) (rest tail))))))

(defn new-nice? [s]
  (and (repeating-letters? s)
       (repeat-with-char-between? s)))

(defn -main [part]
  (let [strings (str/split-lines (slurp "input.txt"))
        nice-fn (case (str part)
                  "1" nice?
                  "2" new-nice?
                  (throw (Exception. "`part` must be 1 or 2")))]
    (println (count (filter nice-fn strings)))))

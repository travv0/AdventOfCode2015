(ns day11.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn increment-password [password]
  (let [reversed-password (reverse password)]
    (letfn [(r [c tail]
              (if (< (int c) (int \z))
                (cons (char (inc (int c)))
                      tail)
                (cons \a (r (first tail) (rest tail)))))]
      (-> (r (first reversed-password)
             (rest reversed-password))
          reverse))))

(defn straight-of-three? [password]
  (let [partitions (partition 3 1 password)]
    (some (fn [cs]
            (let [[a b c] (map int cs)]
              (= [a b c] [a (inc a) (inc (inc a))])))
          partitions)))

(defn excludes-characters? [password chars]
  (not-any? (set chars) password))

(defn positions
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn contains-two-pairs? [password]
  (let [partitions (partition 2 1 password)]
    (->> partitions
         (positions (fn [[a b]] (= a b)))
         (partition 2 1)
         (filter (fn [[x y]] (> (- y x) 1)))
         count
         (<= 1))))

(defn valid? [password]
  (and (excludes-characters? password [\i \o \l])
       (straight-of-three? password)
       (contains-two-pairs? password)))

(defn next-valid-password [password]
  (loop [password (increment-password password)]
    (if (valid? password)
      (str/join password)
      (recur (increment-password password)))))

(defn -main [part]
  (let [input "vzbxkghb"
        new-password (next-valid-password input)]
    (println
     (case (str part)
       "1" new-password
       "2" (next-valid-password new-password)))))

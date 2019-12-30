(ns day13.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as c])
  (:gen-class))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (reduce #(let [[_ person gl happiness-str by-person]
                   (re-matches
                    #"(.+) would (gain|lose) (\d+) happiness units by sitting next to (.+)\."
                    %2)
                   happiness (Integer/parseInt happiness-str)]
               (update %1 person (fn [m]
                                   (assoc m by-person (if (= gl "lose")
                                                        (- happiness)
                                                        happiness)))))
            {}
            lines)))

(defn find-happiness [happiness-map people]
  (let [n (count people)]
    (->> (for [i (range n)]
          [(nth people (mod (dec i) n))
           (nth people i)
           (nth people (mod (inc i) n))])
        (map (fn [[left-person person right-person]]
               (+ (get-in happiness-map [person left-person] 0)
                  (get-in happiness-map [person right-person] 0))))
        (reduce +))))

(defn find-best-happiness [happiness-map]
  (let [combos (c/permutations (keys happiness-map))]
    (reduce (fn [highest-happiness combo]
              (let [happiness (find-happiness happiness-map combo)]
                (max happiness highest-happiness)))
            Integer/MIN_VALUE
            combos)))

(defn -main [part]
  (let [happiness-map (parse-input (slurp "input.txt"))]
    (println
     (case (str part)
       "1" (find-best-happiness happiness-map)
       "2" (find-best-happiness (assoc happiness-map "You" {}))
       (throw (Exception. "`part` must be 1 or 2"))))))

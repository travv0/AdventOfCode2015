(ns day06.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-instructions [input part]
  (let [lines (str/split-lines input)]
    (map #(let [[_ mode start-x start-y end-x end-y]
                (re-matches #"(.+) (\d+),(\d+) through (\d+),(\d+)" %)]
            {:mode mode
             :start-x (Integer/parseInt start-x)
             :start-y (Integer/parseInt start-y)
             :end-x (Integer/parseInt end-x)
             :end-y (Integer/parseInt end-y)})
         lines)))

(defn initialize-grid [x y val]
  (vec (repeat y (vec (repeat x val)))))

(defn update-range [grid update-fn start-x start-y end-x end-y]
  (reduce (fn [grid y]
            (reduce (fn [grid x] (update-in grid [y x] update-fn))
                    grid
                    (range start-x (inc end-x))))
          grid
          (range start-y (inc end-y))))

(defn -main [part]
  (let [instructions (parse-instructions (slurp "input.txt") part)]
    (println (case (str part)
               "1" (->> instructions
                        (reduce #(update-range %1
                                               (case (:mode %2)
                                                 "turn on" (fn [_] 1)
                                                 "turn off" (fn [_] 0)
                                                 "toggle" (fn [x] (mod (inc x) 2)))
                                               (:start-x %2)
                                               (:start-y %2)
                                               (:end-x %2)
                                               (:end-y %2))
                                (initialize-grid 1000 1000 0))
                        flatten
                        (filter (partial = 1))
                        count)
               "2" (->> instructions
                        (reduce #(update-range %1
                                               (case (:mode %2)
                                                 "turn on" inc
                                                 "turn off" (fn [x] (if (> x 0) (dec x) x))
                                                 "toggle" (partial + 2))
                                               (:start-x %2)
                                               (:start-y %2)
                                               (:end-x %2)
                                               (:end-y %2))
                                (initialize-grid 1000 1000 0))
                        flatten
                        (reduce +))
               (throw (Exception. "`part` must be 1 or 2"))))))

(ns day18.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-grid [input]
  (let [lines (str/split-lines input)]
    (map (fn [line] (map #(= \# %) line)) lines)))

(defn get-light [grid x y]
  (try (nth (nth grid y) x)
       (catch Exception e false)))

(defn count-surrounding-lights [grid x y]
  (let [directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
    (->> directions
         (map (fn [[dx dy]] (get-light grid (+ x dx) (+ y dy))))
         (filter identity)
         count)))

(defn part1-check [grid x y _ _]
  (boolean
   (let [light (get-light grid x y)
         number-of-surrounding-lights (count-surrounding-lights grid x y)]
     (or (and light (some #{number-of-surrounding-lights} [2 3]))
         (and (not light) (= number-of-surrounding-lights 3))))))

(defn part2-check [grid x y width height]
  (boolean
   (or (and (= x 0) (= y 0))
       (and (= x 0) (= y (dec height)))
       (and (= x (dec width)) (= y 0))
       (and (= x (dec width)) (= y (dec height)))
       (let [light (get-light grid x y)
             number-of-surrounding-lights (count-surrounding-lights grid x y)]
         (or (and light (some #{number-of-surrounding-lights} [2 3]))
             (and (not light) (= number-of-surrounding-lights 3)))))))

(defn step [check-fn grid]
  (let [height (count grid)
        width (count (nth grid 0))]
    (map (fn [y]
           (map (fn [x] (check-fn grid x y width height))
                (range width)))
         (range height))))

(defn step-times [check-fn grid n]
  (if (<= n 0)
    grid
    (recur check-fn (step check-fn grid) (dec n))))

(defn initialize-grid [grid]
  (let [height (count grid)
        width (count (nth grid 0))]
    (-> (map vec grid)
        vec
        (assoc-in [0 0] true)
        (assoc-in [0 (dec width)] true)
        (assoc-in [(dec height) 0] true)
        (assoc-in [(dec height) (dec width)] true))))

(defn -main [part]
  (let [grid (parse-grid (slurp "input.txt"))
        grid (if (= part 2) (initialize-grid grid) grid)
        check-fn (case part
                   1 part1-check
                   2 part2-check
                   (throw (Exception. "`part` must be 1 or 2")))]
    (count (filter identity (flatten (step-times check-fn grid 100))))))

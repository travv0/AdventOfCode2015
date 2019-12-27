(ns day02.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-dimensions [input]
  (->> input
      str/split-lines
      (map #(let [[_ l w h] (re-matches #"(\d+)x(\d+)x(\d+)" %)]
              {:l (Integer/parseInt l)
               :w (Integer/parseInt w)
               :h (Integer/parseInt h)}))))

(defn calculate-wrapping-paper-size [{:keys [l w h]}]
  (let [sides [(* l w) (* w h) (* h l)]
        smallest-side (apply min sides)]
    (+ (->> (map (partial * 2) sides) (reduce +))
       smallest-side)))

(defn -main [part]
  (let [dimensions (parse-dimensions (slurp "input.txt"))]
    (case part
      1 (->> dimensions
             (map calculate-wrapping-paper-size)
             (reduce +)))))

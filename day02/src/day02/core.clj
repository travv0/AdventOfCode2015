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

(defn calculate-ribbon-length [{:keys [l w h]}]
  (let [[p1 p2] (rest (sort > [l w h]))
        main-length (+ p1 p1 p2 p2)
        bow-length (* l w h)]
    (+ main-length bow-length)))

(defn -main [part]
  (let [dimensions (parse-dimensions (slurp "input.txt"))]
    (println (->> dimensions
                  (map (case (str part)
                         "1" calculate-wrapping-paper-size
                         "2" calculate-ribbon-length
                         (throw (Exception. "`part` must be 1 or 2"))))
                  (reduce +)))))

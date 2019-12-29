(ns day09.core
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as str])
  (:gen-class))

(defn get-shortest-route [nodes edges]
  (let [all-options (c/permutations nodes)]
    (loop [option (first all-options)
           rest-options (rest all-options)
           min-distance Integer/MAX_VALUE]
      (if (nil? option)
        min-distance
        (let [route (partition 2 1 option)
              distance (->> route
                            (map #(get edges (set %)))
                            (reduce +))]
          (recur (first rest-options)
                 (rest rest-options)
                 (min distance min-distance)))))))

(defn get-longest-route [nodes edges]
  (let [all-options (c/permutations nodes)]
    (loop [option (first all-options)
           rest-options (rest all-options)
           max-distance 0]
      (if (nil? option)
        max-distance
        (let [route (partition 2 1 option)
              distance (->> route
                            (map #(get edges (set %)))
                            (reduce +))]
          (recur (first rest-options)
                 (rest rest-options)
                 (max distance max-distance)))))))

(defn parse-edges [input]
  (let [lines (str/split-lines input)]
    (reduce (fn [map line]
              (let [[from _ to _ distance] (str/split line #" ")]
                (assoc map #{from to} (Integer/parseInt distance))))
            {}
            lines)))

(defn edges->nodes [edges]
  (reduce into #{} (keys edges)))

(defn -main [part]
  (let [edges (parse-edges (slurp "input.txt"))
        nodes (edges->nodes edges)]
    (println
     (case (str part)
       "1" (get-shortest-route nodes edges)
       "2" (get-longest-route nodes edges)
       (throw (Exception. "`part` must be 1 or 2"))))))

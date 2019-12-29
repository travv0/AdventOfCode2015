(ns day09.core
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as str])
  (:gen-class))

(defn get-route-distance [nodes edges f start-distance]
  (let [all-options (c/permutations nodes)]
    (loop [option (first all-options)
           rest-options (rest all-options)
           most-distance start-distance]
      (if (nil? option)
        most-distance
        (let [route (partition 2 1 option)
              distance (->> route
                            (map #(get edges (set %)))
                            (reduce +))]
          (recur (first rest-options)
                 (rest rest-options)
                 (f distance most-distance)))))))

(defn get-shortest-route [nodes edges]
  (get-route-distance nodes edges min Integer/MAX_VALUE))

(defn get-longest-route [nodes edges]
  (get-route-distance nodes edges max 0))

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

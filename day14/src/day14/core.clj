(ns day14.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (map #(let [[_ name kmps for-secs rest-secs]
                (re-matches
                 #"(.+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
                 %)]
            {:name name
             :speed (Integer/parseInt kmps)
             :fly-time (Integer/parseInt for-secs)
             :rest-time (Integer/parseInt rest-secs)})
         lines)))

(defn distance-after [seconds reindeer]
  (loop [seconds seconds
         remaining-fly-time (:fly-time reindeer)
         remaining-rest-time (:rest-time reindeer)
         distance 0]
    (if (> seconds 0)
      (recur (dec seconds)
             (cond (> remaining-fly-time 0) (dec remaining-fly-time)
                   (<= remaining-rest-time 1) (:fly-time reindeer)
                   :else 0)
             (if (and (> remaining-rest-time 0)
                      (= remaining-fly-time 0))
               (dec remaining-rest-time)
               (:rest-time reindeer))
             (+ distance (if (> remaining-fly-time 0) (:speed reindeer) 0)))
      distance)))

(defn -main [part]
  (let [reindeer (parse-input (slurp "input.txt"))]
    (println
     (case (str part)
       "1" (reduce max (map (partial distance-after 2503) reindeer))
       "2" (throw (Exception. "unimplemented"))
       (throw (Exception. "`part` must be 1 or 2"))))))

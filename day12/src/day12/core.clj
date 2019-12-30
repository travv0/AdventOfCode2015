(ns day12.core
  (:require [clojure.data.json :as json]
            [com.rpl.specter :as s])
  (:gen-class))

(defn sum-numbers-in-json [json]
  (->> json
       (s/select (s/walker number?))
       (reduce +)))

(defn contains-red? [map]
  (when (map? map)
    (some (comp #(= "red" %) second) map)))

(defn sum-without-reds [json]
  (->> json
       (s/transform (s/walker contains-red?) s/NONE)
       sum-numbers-in-json))

(defn -main [part]
  (let [json (json/read-str (slurp "input.txt"))]
    (println
     (case (str part)
       "1" (sum-numbers-in-json json)
       "2" (sum-without-reds json)
       (throw (Exception. "`part` must be 1 or 2"))))))

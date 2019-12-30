(ns day12.core
  (:gen-class))

(defn sum-numbers-in-json [json]
  (->> json
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))
       (reduce +)))

(defn -main [part]
  (let [input (slurp "input.txt")]
    (println
     (case (str part)
       "1" (sum-numbers-in-json input)
       "2" (throw (Exception. "unimplemented"))
       (throw (Exception. "`part` must be 1 or 2"))))))

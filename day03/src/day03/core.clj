(ns day03.core
  (:gen-class))

(defn parse-directions [input]
  (map #(case %
          \^ [0 -1]
          \> [1 0]
          \< [-1 0]
          \v [0 1])
       input))

(defn get-route [directions]
  (reductions (fn [[x y] [dx dy]] [(+ x dx) (+ y dy)])
              [0 0]
              directions))

(defn -main []
  (let [directions (parse-directions (slurp "input.txt"))]
    (-> directions get-route set count)))

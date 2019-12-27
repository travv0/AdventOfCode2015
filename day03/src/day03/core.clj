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

(defn split-directions [directions]
  (reduce (fn [[santa1 santa2] [dir1 dir2]]
            [(conj santa1 dir1) (conj santa2 dir2)])
          [[] []]
          (partition-all 2 directions)))

(defn -main [part]
  (let [directions (parse-directions (slurp "input.txt"))]
    (case part
      1 (-> directions get-route set count)
      2 (->> directions
             split-directions
             (map get-route)
             (reduce concat)
             set
             count)
      (throw (Exception. "`part` must be 1 or 2")))))

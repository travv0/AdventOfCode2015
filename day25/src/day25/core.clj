(ns day25.core
  (:gen-class))

(defn calculate-value [previous-value]
  (rem (* previous-value 252533) 33554393))

(defn generate-indices []
  (mapcat
   #(let [xs (range 1 %)
          ys (reverse (range 1 %))]
      (map vector xs ys))
   (rest (range))))

(defn generate-table []
  (map vector (generate-indices) (iterate calculate-value 20151125)))

(defn get-code [row column]
  (-> (filter (fn [[[x y] _]] (and (= x column) (= y row)))
              (generate-table))
      first
      second))

(defn -main [part]
  (let [row 2947, column 3029]
    (case part
      1 (get-code row column)
      2 (throw (Exception. "no part 2 for day 25"))
      (throw (Exception. "`part` must be 1 or 2")))))

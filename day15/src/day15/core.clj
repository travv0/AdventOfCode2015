(ns day15.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-line [line]
  (let [ingredient (second (re-matches #"(\w+):.*" line))
        qualities (into {} (map (fn [[_ quality score]]
                                  [quality (Integer/parseInt score)])
                                (re-seq #"(\w+) (-?\d+)" line)))]
    {ingredient qualities}))

(defn parse-ingredients [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into {})))

(defn calculate-score-for-quality [ingredients amounts quality]
  (->> amounts
       (map (fn [[ingredient amount]]
              (* amount (get-in ingredients [ingredient quality]))))
       (reduce +')
       (max 0)))

(defn calculate-cookie-score
  ([ingredients amounts]
   (let [qualities ["capacity" "durability" "flavor" "texture"]]
     (->> qualities
          (map (partial calculate-score-for-quality ingredients amounts))
          (reduce *'))))
  ([calories ingredients amounts]
   (if (= calories (calculate-score-for-quality ingredients amounts "calories"))
     (calculate-cookie-score ingredients amounts)
     0)))

(defn find-ingredient-options [ingredient-count total-count]
  (let [f (fn f [options n]
            (cond
              (= (count options) (dec ingredient-count)) [(conj options
                                                                (- total-count
                                                                   (reduce +' options)))]
              :else (mapcat #(f (conj options %) (- n %))
                            (range 1 (- total-count (or (last options) 0))))))]
    (f [] total-count)))

(defn find-best-cookie-score [cookie-score-func ingredients]
  (let [amount-options (find-ingredient-options (count ingredients) 100)]
    (reduce max
            (map #(cookie-score-func ingredients
                                     (into {} (map (fn [ingredient score] [ingredient score])
                                                   (keys ingredients)
                                                   %)))
                 amount-options))))

(defn -main [part]
  (let [ingredients (parse-ingredients (slurp "input.txt"))]
    (case part
      1 (find-best-cookie-score calculate-cookie-score ingredients)
      2 (find-best-cookie-score (partial calculate-cookie-score 500) ingredients)
      (throw (Exception. "`part` must be 1 or 2")))))

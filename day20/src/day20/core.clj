(ns day20.core
  (:gen-class))

(defn factors [number]
  (distinct
   (flatten
    (for [divisor (range 1 (inc (Math/floor (Math/sqrt number))))
          :when (= 0 (mod number divisor))]
      [divisor (quot number divisor)]))))

(defn calculate-presents [predicate presents house]
  (->> (factors house)
       (filter (partial predicate house))
       (reduce +)
       (* presents)))

(defn find-house-with-presents [predicate presents-per-house presents]
  (->> (range)
       (pmap (partial calculate-presents predicate presents-per-house))
       (pmap #(hash-map :house %1 :presents %2) (range))
       (filter #(>= (:presents %) presents))
       first
       :house))

(defn -main [part]
  (let [min-presents 36000000]
    (case part
      1 (find-house-with-presents (constantly true) 10 min-presents)
      2 (find-house-with-presents #(<= %1 (* 50 %2)) 11 min-presents)
      (throw (Exception. "`part` must be 1 or 2")))))

(ns day10.core
  (:gen-class))

(defn digits [number]
  (map #(Character/digit % 10) (str number)))

(defn look-and-say [digits]
  (->> digits
       (partition-by identity)
       (map #(do [(count %) (first %)]))
       flatten))

(defn do-look-and-says [n digits]
  (loop [n n, digits digits]
    (if (> n 0)
      (recur (dec n) (look-and-say digits))
      digits)))

(defn -main [part]
  (let [input 3113322113]
    (println
     (case (str part)
       "1" (count (do-look-and-says 40 (digits input)))
       "2" (count (do-look-and-says 50 (digits input)))
       (throw (Exception. "`part` must be 1 or 2"))))))

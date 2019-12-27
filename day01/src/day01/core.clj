(ns day01.core
  (:gen-class))

(defn -main [part]
  (let [input (slurp "input.txt")]
    (loop [c (first input)
           str (rest input)
           i 0
           story 0]
      (cond (and (= part 2) (= story -1)) i
            (nil? c) story
            :else (recur (first str)
                         (rest str)
                         (inc i)
                         (case c
                           \( (inc story)
                           \) (dec story)))))))

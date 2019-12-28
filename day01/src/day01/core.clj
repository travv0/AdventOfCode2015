(ns day01.core
  (:gen-class))

(defn -main [part]
  (let [input (slurp "input.txt")]
    (println (loop [head (first input)
                    tail (rest input)
                    i 0
                    story 0]
               (cond (and (= (str part) "2") (= story -1)) i
                     (nil? head) story
                     :else (recur (first tail)
                                  (rest tail)
                                  (inc i)
                                  (case head
                                    \( (inc story)
                                    \) (dec story))))))))

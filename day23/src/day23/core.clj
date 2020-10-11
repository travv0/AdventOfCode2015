(ns day23.core
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def computer
  {:registers {"a" 0 "b" 0}
   :instruction 0})

(defn jmp [offset computer]
  (update computer :instruction #(+ % (Integer/parseInt offset))))

(defn hlf [r computer]
  (->> (update-in computer [:registers r] #(/ % 2))
       (jmp "1")))

(defn tpl [r computer]
  (->> (update-in computer [:registers r] #(* % 3))
       (jmp "1")))

(defn incr [r computer]
  (->> (update-in computer [:registers r] inc)
       (jmp "1")))

(defn jie [r offset computer]
  (if (even? (get-in computer [:registers r]))
    (jmp offset computer)
    (jmp "1" computer)))

(defn jio [r offset computer]
  (if (= (get-in computer [:registers r]) 1)
    (jmp offset computer)
    (jmp "1" computer)))

(defn parse-instructions [input]
  (for [line (str/split-lines input)]
    (match
     (str/split line #",? ")
     ["hlf" r] (partial hlf r)
     ["tpl" r] (partial tpl r)
     ["inc" r] (partial incr r)
     ["jmp" offset] (partial jmp offset)
     ["jie" r offset] (partial jie r offset)
     ["jio" r offset] (partial jio r offset))))

(defn run-instruction [computer instructions]
  (let [instruction (:instruction computer)]
    ((nth instructions instruction) computer)))

(defn run-instructions [computer instructions]
  (if (>= (:instruction computer) (count instructions))
    computer
    (run-instructions (run-instruction computer instructions)
                      instructions)))

(defn -main [part]
  (let [input (slurp "input.txt")
        instructions (parse-instructions input)]
    (case part
      1 (run-instructions computer instructions)
      2 (throw (Exception. "part 2 not yet implemented"))
      (throw (Exception. "`part` must be 1 or 2")))))

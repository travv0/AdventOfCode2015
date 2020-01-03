(ns day16.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-line [line]
  (let [[sue things] (map str/join (split-at (str/index-of line \:) line))
        sue-num (->> sue (re-find #"Sue (\d+)") second Integer/parseInt)]
    [sue-num (-> things (subs 2) parse-aunt-things)]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-line)))

(defn parse-aunt-things [input]
  (as-> input <>
    (str/split <> #"(, )|\n")
    (map #(let [[thing num] (str/split % #": ")]
            [thing (Integer/parseInt num)])
         <>)
    (into {} <>)))

(defn find-matching-aunt [aunts goal-aunt pred]
  (loop [[aunt things] (first aunts)
         aunts (rest aunts)]
    (if (every? pred things)
      aunt
      (recur (first aunts) (rest aunts)))))

(def goal-aunt-input "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1")

(defn -main [part]
  (let [aunts (parse-input (slurp "input.txt"))
        goal-aunt (parse-aunt-things goal-aunt-input)]
    (println
     (case (str part)
       "1" (find-matching-aunt aunts goal-aunt
                               (fn [[thing num]]
                                 (= num (get goal-aunt thing))))
       "2" (find-matching-aunt aunts goal-aunt
                               (fn [[thing num]]
                                 (case thing
                                   ("cats" "trees") (> num (get goal-aunt thing))
                                   ("pomeranians" "goldfish") (< num (get goal-aunt thing))
                                   (= num (get goal-aunt thing)))))
       (throw (Exception. "`part` must be 1 or 2"))))))

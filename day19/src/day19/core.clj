(ns day19.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn find-replacements [molecule replacements]
  (distinct
   (loop [before "" curr molecule new-molecules []]
     (if (empty? curr)
       new-molecules
       (recur (str before (first curr))
              (str/join (rest curr))
              (remove nil?
                      (into new-molecules
                            (for [[from to] replacements]
                              (when (str/starts-with? curr from)
                                (str before to (str/join (drop (count from) curr))))))))))))

(defn parse-replacements [input]
  (map #(str/split % #" => ")
       (drop-last 2 (str/split-lines input))))

(defn parse-molecule [input]
  (last (str/split-lines input)))

(defn find-fewest-steps-from-e [molecule replacements]
  (letfn [(f [molecule replacements step]
            (if (= molecule "e")
              step
              (->> (find-replacements molecule replacements)
                   (map #(f % replacements (inc step)))
                   (some identity))))]
    (f molecule (map reverse replacements) 0)))

(defn -main [part]
  (let [input (slurp "input.txt")
        replacements (parse-replacements input)
        molecule (parse-molecule input)]
    (case part
      1 (count (find-replacements molecule replacements))
      2 (find-fewest-steps-from-e molecule replacements)
      (throw (Exception. "`part` must be 1 or 2")))))

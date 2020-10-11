(ns day24.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as c]
            [clojure.set :as set]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (str/split-lines input)))

(defn goal-weight [packages groups]
  (/ (reduce + packages) groups))

(defn make-groups
  ([packages groups]
   (make-groups packages groups (goal-weight packages groups)))
  ([packages groups goal-weight]
   (let [packages (sort > packages)]
     (letfn [(make-groups* [[package & packages] groups group group-total]
               (cond (nil? package) []

                     (= (+ package group-total) goal-weight)
                     (conj groups (conj group package))

                     (< (+ package group-total) goal-weight)
                     (recur packages
                            (concat groups (distinct (make-groups* packages groups group group-total)))
                            (conj group package)
                            (+ group-total package))

                     :else (recur packages groups group group-total)))]
       (make-groups* packages [] [] 0)))))

(defn calculate-quantum-entanglement [packages groups]
  (let [goal-weight (goal-weight packages groups)
        passenger-packages (->> (make-groups packages groups)
                                (filter #(= (reduce + %) goal-weight)))
        min-count (reduce #(min %1 (count %2))
                          (count (first passenger-packages))
                          (rest passenger-packages))
        balanced-packages (filter #(= (count %) min-count) passenger-packages)]
    (reduce #(min %1 (reduce * %2))
            (reduce * (first balanced-packages))
            (rest balanced-packages))))

(defn -main [part]
  (let [input (slurp "input.txt")
        packages (parse-input input)]
    (case part
      1 (calculate-quantum-entanglement packages 3)
      2 (calculate-quantum-entanglement packages 4)
      (throw (Exception. "`part` must be 1 or 2")))))

(ns day17.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn maplist
  ([s] (maplist identity s))
  ([f s] (when-let [s (seq s)] (lazy-seq (cons (f s) (maplist f (next s)))))))

(defn find-all-container-combos [containers num]
  (let [f (fn f [arr containers reduced-num]
            (if (= reduced-num 0)
              [arr]
              (apply concat (maplist #(f (conj arr (first %))
                                         %
                                         (- reduced-num (first %)))
                                     (rest containers)))))]
    (apply concat (maplist #(f [(first %)] % (- num (first %)))
                           containers))))

(defn parse-containers [input]
  (map #(Integer/parseInt %) (str/split-lines input)))

(defn -main [part]
  (let [containers (parse-containers (slurp "input.txt"))
        combos (find-all-container-combos containers 150)]
    (case part
      1 (count combos)
      2 (let [min-containers (reduce min (map count combos))]
          (count (filter #(= (count %) min-containers) combos)))
      (throw (Exception. "`part` must be 1 or 2")))))

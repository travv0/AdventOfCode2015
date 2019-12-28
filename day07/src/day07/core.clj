(ns day07.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [weavejester.dependency :as dep])
  (:gen-class))

(defn maybe-get-wire [map key]
  (try (Integer/parseInt key)
       (catch NumberFormatException _ (get map key))))

(defn reduce-line [map line]
  (match (str/split line #" ")
         [d "->" r] (assoc map r {:wire r
                                  :deps #{d}
                                  :fn (fn [m] (assoc m r (maybe-get-wire m d)))})
         [x "AND" y "->" r] (assoc map r {:wire r
                                          :deps #{x y}
                                          :fn (fn [m] (assoc m r (bit-and (maybe-get-wire m x)
                                                                          (maybe-get-wire m y))))})
         [x "OR" y "->" r] (assoc map r {:wire r
                                         :deps #{x y}
                                         :fn (fn [m] (assoc m r (bit-or (maybe-get-wire m x)
                                                                        (maybe-get-wire m y))))})
         [x "LSHIFT" n "->" r] (assoc map r {:wire r
                                             :deps #{x}
                                             :fn (fn [m]
                                                   (assoc m r (bit-shift-left
                                                               (maybe-get-wire m x)
                                                               (Integer/parseInt n))))})
         [x "RSHIFT" n "->" r] (assoc map r {:wire r
                                             :deps #{x}
                                             :fn (fn [m]
                                                   (assoc m r (bit-shift-right
                                                               (maybe-get-wire m x)
                                                               (Integer/parseInt n))))})
         ["NOT" x "->" r] (assoc map r {:wire r
                                        :deps #{x}
                                        :fn (fn [m] (assoc m r (bit-not (maybe-get-wire m x))))})))

(defn parse-circuit [input]
  (let [lines (str/split-lines input)]
    (as-> lines <>
      (reduce reduce-line {} <>)
      (map (fn [[wire-id wire]]
             [wire-id (update wire :deps
                              (fn [deps]
                                (filter (fn [dep]
                                          (try (Integer/parseInt dep)
                                               false
                                               (catch NumberFormatException _ true)))
                                        deps)))])
           <>)
      (into {} <>))))

(defn build-graph [circuit]
  (reduce #(let [wire (maybe-get-wire circuit %2)]
             (reduce (fn [g dep] (dep/depend g wire (maybe-get-wire circuit dep)))
                     %1
                     (:deps wire)))
          (dep/graph)
          (keys circuit)))

(defn emulate-circuit [graph]
  (reduce #(when %2
             ((:fn %2) %1))
          {}
          (dep/topo-sort graph)))

(defn -main []
  (let [input (slurp "input.txt")]
    (get (-> input
             parse-circuit
             build-graph
             emulate-circuit)
         "a")))

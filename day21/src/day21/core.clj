(ns day21.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as c]))

(def shop [{:type :weapon, :name "Dagger",     :cost   8, :damage 4, :armor 0}
           {:type :weapon, :name "Shortsword", :cost  10, :damage 5, :armor 0}
           {:type :weapon, :name "Warhammer",  :cost  25, :damage 6, :armor 0}
           {:type :weapon, :name "Longsword",  :cost  40, :damage 7, :armor 0}
           {:type :weapon, :name "Greataxe",   :cost  74, :damage 8, :armor 0}

           {:type :armor,  :name "Leather",    :cost  13, :damage 0, :armor 1}
           {:type :armor,  :name "Chainmail",  :cost  31, :damage 0, :armor 2}
           {:type :armor,  :name "Splintmail", :cost  53, :damage 0, :armor 3}
           {:type :armor,  :name "Bandedmail", :cost  75, :damage 0, :armor 4}
           {:type :armor,  :name "Platemail",  :cost 102, :damage 0, :armor 5}

           {:type :ring,   :name "Damage +1",  :cost  25, :damage 1, :armor 0}
           {:type :ring,   :name "Damage +2",  :cost  50, :damage 2, :armor 0}
           {:type :ring,   :name "Damage +3",  :cost 100, :damage 3, :armor 0}
           {:type :ring,   :name "Defense +1", :cost  20, :damage 0, :armor 1}
           {:type :ring,   :name "Defense +2", :cost  40, :damage 0, :armor 2}
           {:type :ring,   :name "Defense +3", :cost  80, :damage 0, :armor 3}])

(def equipment [{:type :weapon, :name "Shortsword", :cost  10, :damage 5, :armor 0}
                {:type :armor,  :name "Platemail",  :cost 102, :damage 0, :armor 5}])

(defn get-stat [equipment stat]
  (reduce + (map #(get % stat 0) equipment)))

(defn make-player [hit-points equipment]
  {:hit-points hit-points
   :damage (get-stat equipment :damage)
   :armor (get-stat equipment :armor)
   :equipment equipment})

(def boss {:hit-points 104
           :damage 8
           :armor 1})

(defn attack [attacker target]
  (let [damage (:damage attacker)
        armor (:armor target)]
    (update target :hit-points #(- % (- damage armor)))))

(defn run-turn [player boss]
  (let [boss (attack player boss)
        player (attack boss player)]
    [player boss]))

(defn run-game [player boss]
  (cond (<= (:hit-points boss) 0) {:winner :player :player player :boss boss}
        (<= (:hit-points player) 0) {:winner :boss :player player :boss boss}
        :else (let [[player boss] (run-turn player boss)]
                (recur player boss))))

(def weapon-options (filter #(= :weapon (:type %)) shop))

(def armor-options (conj (filter #(= :armor (:type %)) shop)
                         nil))

(def ring-options
  (map (partial filter identity)
       (c/combinations (conj (filter #(= :ring (:type %)) shop)
                             nil
                             nil)
                       2)))

(def equipment-options
  (map (comp (partial filter identity) flatten)
       (c/cartesian-product weapon-options armor-options ring-options)))

(defn get-player-cost-by-winner [equipment-options winner pred boss]
  (let [players (map (partial make-player 100) equipment-options)
        results (map #(run-game % boss) players)
        winner-victories (filter #(= (:winner %) winner) results)
        equipment-costs (map (comp #(get-stat % :cost) :equipment :player) winner-victories)]
    (reduce pred equipment-costs)))

(defn -main [part]
  (case part
    1 (get-player-cost-by-winner equipment-options :player min boss)
    2 (get-player-cost-by-winner equipment-options :boss max boss)
    (throw (Exception. "`part` must be 1 or 2"))))

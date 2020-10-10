(ns day22.core
  (:gen-class)
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def state
  {:player {:hit-points 50
            :mana 500
            :armor 0
            :spell nil}
   :boss {:hit-points 55
          :damage 8}
   :effects []
   :turn :player})

(defn damage [receiver amount state]
  (let [armor (get-in state [receiver :armor] 0)
        damage (- amount armor)]
    (update-in state [receiver :hit-points]
               #(- % (if (< damage 1) 1 damage)))))

(defn heal [receiver amount state]
  (update-in state [receiver :hit-points] #(+ % amount)))

(def timer-for
  {:shield 6
   :poison 6
   :recharge 5})

(defn start-effect [effect-name state]
  (update state :effects #(conj % {:name effect-name :timer (timer-for effect-name)})))

(def spells {:magic-missile {:mana 53 :func (partial damage :boss 4)}
             :drain {:mana 73 :func #(->> (damage :boss 2 %)
                                          (heal :player 2))}
             :shield {:mana 113 :func (partial start-effect :shield)}
             :poison {:mana 173 :func (partial start-effect :poison)}
             :recharge {:mana 229 :func (partial start-effect :recharge)}})

(defn cast [spell-name state]
  (-> ((get-in spells [spell-name :func]
               (fn [& _] (throw (Exception. (str "bad spell name " spell-name)))))
       state)
      (update-in [:player :mana] #(- % (get-in spells [spell-name :mana] 0)))
      (assoc-in [:player :spell] spell-name)))

(def effect-func
  {:shield (fn [state] (assoc-in state [:player :armor] 7))
   :poison (fn [state] (damage :boss 3 state))
   :recharge (fn [state] (update-in state [:player :mana] #(+ % 101)))})

(defn run-effects [state]
  (letfn [(run-effects [state used-effects]
            (if (empty? (:effects state))
              (assoc state :effects (filter #(pos? (:timer %)) used-effects))
              (let [effect (first (:effects state))
                    f (effect-func (:name effect))
                    state (-> state
                              f
                              (update :effects rest))
                    used-effects (conj used-effects (update effect :timer dec))]
                (run-effects state used-effects))))]
    (-> state
        (assoc-in [:player :armor] 0)
        (assoc-in [:player :spell] nil)
        (run-effects []))))

(defn run-turn [spell-name difficulty state]
  (let [state (run-effects state)]
    (case (:turn state)
      :player (as-> state <>
                (if (= difficulty :hard)
                  (damage :player 1 <>)
                  <>)
                (cast spell-name <>)
                (assoc <> :turn :boss))
      :boss (-> (damage :player 8 state)
                (assoc :turn :player)))))

(defn generate-route [node came-from]
  (loop [route '()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      route)))

(defn route [graph dist h start goal?]
  (loop [visited {}
         queue (priority-map-keyfn first start [0 0 nil])]
    (when (seq queue)
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (goal? current)
          (generate-route current visited)
          (recur visited (reduce (fn [queue node]
                                   (let [score (+ current-score (dist current node))]
                                     (if (and (not (contains? visited node))
                                              (or (not (contains? queue node))
                                                  (< score (get-in queue [node 1]))))
                                       (assoc queue node [(+ score (h node)) score current])
                                       queue)))
                                 (pop queue)
                                 (graph current))))))))

(defn state-valid? [difficulty state]
  (and (or (> (get-in state [:player :hit-points])
              (if (= difficulty :hard) 1 0))
           (not (pos? (get-in state [:boss :hit-points]))))
       (pos? (get-in state [:player :mana]))
       (or (empty? (:effects state))
           (apply distinct? (map :name (:effects state))))))

(defn next-states [difficulty state]
  (let [potential-states (if (= (:turn state) :player)
                           (map #(run-turn % difficulty state) (keys spells))
                           [(run-turn nil difficulty state)])]
    (filter (partial state-valid? difficulty) potential-states)))

(defn get-mana [state]
  (-> (get-in state [:player :spell])
      spells
      (get :mana 0)))

(defn dist [from to]
  (get-mana to))

(defn goal? [state]
  (not (pos? (get-in state [:boss :hit-points]))))

(defn h [state]
  (get-in state [:boss :hit-points]))

(defn get-min-mana-to-win [state difficulty]
  (->> (route (partial next-states difficulty) dist h state goal?)
       (map get-mana)
       (reduce +)))

(defn -main [part]
  (case part
    1 (get-min-mana-to-win state :normal)
    2 (get-min-mana-to-win state :hard)
    (throw (Exception. "`part` must be 1 or 2"))))

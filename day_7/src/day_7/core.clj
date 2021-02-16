(ns day-7.core
  (:gen-class)
  (:require [aoc-2015.core :as aoc]
            [clojure.string :as str]))

(defn re-matches-initial-variable [s]
  (re-matches #"^\d+ -> \w+" s))

(defn re-matches-not [s]
  (re-matches #"NOT \w+ -> \w+" s))

(defn re-matches-operation [s]
  (re-matches #"\w+ [A-Z]+ \w+ -> \w+" s))

(defn set-initial-variable [s]
  (let [s-list (str/split s #" ")
        v (Integer/parseInt (first s-list))
        r (last s-list)]
    {r v}))

(defn set-not-map [s]
  (let [s-list (str/split s #" ")
        x (get s-list 1)
        r (last s-list)]
    {:op bit-not :x x :r r}))

(defn case-logic-gate [s]
  (case s
    "AND" bit-and
    "OR" bit-or
    "RSHIFT" bit-shift-right
    "LSHIFT" bit-shift-left
    nil))

(defn set-operation-map [s]
  (let [s-list (str/split s #" ")
        x (first s-list)
        y (get s-list 2)
        r (last s-list)
        raw-op (get s-list 1)
        op (case-logic-gate raw-op)]
    {:op op :x x :y y :r r}))

(defn op-map-from-string [s]
  (cond
    (re-matches-initial-variable s) [:vars (set-initial-variable s)]
    (re-matches-not s) [:nots (set-not-map s)]
    (re-matches-operation s) [:ops (set-operation-map s)]
    :else nil))

(defn add-op-map-to-nested-maps [m [k v]]
  (let [old-v (if (empty? m) {} (get m k))
        value-to-add (conj old-v v)]
    (-> m (dissoc k) (assoc k value-to-add))))

(defn raw-operation-to-general-map [m s]
  (->> s
       op-map-from-string
       (add-op-map-to-nested-maps m)))

(defn raw-strings-to-general-map [raw-strings]
  (->> raw-strings
       (reduce raw-operation-to-general-map {})))

(defn solve-op [vars {op :op x :x y :y r :r}]
  (if (every? #(contains? vars %) [x y])
    (assoc vars r (op (get vars x) (get vars y)))
    vars))

(defn solve-not [vars {op :op x :x r :r}]
  (if (contains? vars x)
    (assoc vars r (op (get vars x)))
    vars))

(defn contains-evaluated-op? [vars {x :x y :y r :r}]
  (every? #(contains? vars %) [x y r]))

(defn contains-evaluated-not? [vars {x :x r :r}]
  (every? #(contains? vars %) [x r]))

(defn filter-evaluated [{vars :vars ops :ops nots :nots}]
  {:vars vars
   :ops (filter #(not (contains-evaluated-op? vars %)) ops)
   :nots (filter #(not (contains-evaluated-not? vars %)) nots)})

(defn solve-all [{vars :vars ops :ops nots :nots}]
  (flatten [(map #(solve-op vars %) ops) (map #(solve-not vars %) nots)]))

(defn solve-iteration [m]
  (let [{vars :vars ops :ops nots :nots} m]
    (->> m
         (solve-all)
         (apply merge)
         (#(filter-evaluated {:vars % :ops ops :nots nots})))))

(defn solve-iterations [m]
  (loop [om m]
    (let [nm (solve-iteration om)
          {vars :vars ops :ops nots :nots} nm]
      (if
       (or (every? #(empty? %) [ops nots]) (= om nm))
        vars
        (recur nm)))))

(defn get-and-split []
  (-> (aoc/get-challenge 7)
      (str/split #"\n")))

(defn solve1 []
  (-> (get-and-split)
      (raw-strings-to-general-map)
      (solve-iterations)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

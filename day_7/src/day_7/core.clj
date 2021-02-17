(ns day-7.core
  (:gen-class)
  (:require [aoc-2015.core :as aoc]
            [clojure.string :as str]))

(defn re-matches-initial-variable [s]
  (re-matches #"^\d+ -> \w+" s))

(defn re-matches-not [s]
  (re-matches #"NOT \w+ -> \w+" s))

(defn re-matches-operation [s]
  (first (re-matches #"\w+ (AND|OR) \w+ -> \w+" s)))

(defn re-matches-shift [s]
  (first (re-matches #"\w+ (LSHIFT|RSHIFT) \w+ -> \w+" s)))

(defn set-initial-variable [s]
  (let [s-list (str/split s #" ")
        [sv _ r] s-list
        v (Integer/parseInt sv)]
    {r v}))

(defn set-not-map [s]
  (let [s-list (str/split s #" ")
        [_ x _ r] s-list]
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
        [x raw-op y _ r] s-list
        op (case-logic-gate raw-op)
        nx (if (every? #(Character/isDigit %) x) (Integer/parseInt x) nil)]
    (if (= nx nil)
      {:op op :x x :y y :r r}
      {:op op :n nx :y y :r r})))

(defn set-shift-map [s]
  (let [s-list (str/split s #" ")
        [x raw-op sn _ r] s-list
        n (Integer/parseInt sn)
        op (case-logic-gate raw-op)]
    {:op op :x x :n n :r r}))

(defn op-map-from-string [s]
  (cond
    (re-matches-initial-variable s) [:vars (set-initial-variable s)]
    (re-matches-not s) [:nots [(set-not-map s)]]
    (re-matches-operation s) [:ops [(set-operation-map s)]]
    (re-matches-shift s) [:shifts [(set-shift-map s)]]
    :else nil))

(defn collection-from-key [k]
  (if (= k :vars)
    {}
    []))

(defn add-op-map-to-nested-maps [m [k v]]
  (let [old-v (if (contains? m k) (get m k) (collection-from-key k))
        value-to-add (into old-v v)]
    (-> m (dissoc k) (assoc k value-to-add))))

(defn raw-operation-to-general-map [m s]
  (->> s
       op-map-from-string
       (add-op-map-to-nested-maps m)))

(defn raw-strings-to-general-map [raw-strings]
  (->> raw-strings
       (reduce raw-operation-to-general-map {})))

(defn solve-op [vars {op :op x :x y :y n :n r :r}]
  (if (nil? n)
    (if (every? #(contains? vars %) [x y])
      (assoc vars r (op (get vars x) (get vars y)))
      vars)
    (if (contains? vars y)
      (assoc vars r (op n (get vars y)))
      vars)))

(defn solve-shift [vars {op :op x :x n :n r :r}]
  (if (contains? vars x)
    (assoc vars r (op (get vars x) n))
    vars))

(defn solve-not [vars {op :op x :x r :r}]
  (if (contains? vars x)
    (assoc vars r (op (get vars x)))
    vars))

(defn contains-evaluated-op? [vars {x :x y :y n :n r :r}]
  (if (nil? n)
    (every? #(contains? vars %) [x y r])
    (every? #(contains? vars %) [y r])))

(defn contains-evaluated-not? [vars {x :x r :r}]
  (every? #(contains? vars %) [x r]))

(defn contains-evaluated-shift? [vars {x :x _ :n r :r}]
  (every? #(contains? vars %) [x r]))

(defn filter-evaluated [{vars :vars ops :ops nots :nots shifts :shifts}]
  {:vars vars
   :ops (filter #(not (contains-evaluated-op? vars %)) ops)
   :nots (filter #(not (contains-evaluated-not? vars %)) nots)
   :shifts (filter #(not (contains-evaluated-shift? vars %)) shifts)})

(defn solve-all [{vars :vars ops :ops nots :nots shifts :shifts}]
  (flatten [(map #(solve-op vars %) ops)
            (map #(solve-not vars %) nots)
            (map #(solve-shift vars %) shifts)]))

(defn solve-iteration [m]
  (let [{_ :vars ops :ops nots :nots shifts :shifts} m]
    (->> m
         (solve-all)
         (apply merge)
         (#(filter-evaluated {:vars % :ops ops :nots nots :shifts shifts})))))

(defn solve-iterations [m]
  (loop [om m]
    (let [nm (solve-iteration om)
          {vars :vars ops :ops nots :nots shifts :shifts} nm]
      (if
       (or (every? #(empty? %) [ops nots shifts]) (= om nm))
        vars
        (recur nm)))))

(defn get-and-split []
  (-> (aoc/get-challenge 7)
      (str/split #"\n")))

(defn solve1 []
  (-> (get-and-split)
      (raw-strings-to-general-map)
      (solve-iterations)
      (get "lx")))

(defn solve2 []
  (-> (aoc/get-challenge "7-2")
      (str/split #"\n")
      (raw-strings-to-general-map)
      (solve-iterations)
      (get "lx")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

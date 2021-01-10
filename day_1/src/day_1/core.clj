(ns day-1.core
  (:require [aoc-2015.core :as aoc]
            [clojure.string :as str]))

(defn stair-to-number [stair]
  (cond
    (= stair "(") 1
    (= stair ")") -1
    :else 0))

(defn calc-stairs [stair-list]
  (->> stair-list
       (map stair-to-number)
       (reduce +)))

(defn get-floor-as-number [text]
  (-> text
      (str/split #"")))

(defn solve []
  (-> 1
      aoc/get-challenge
      get-floor-as-number
      calc-stairs))

(defn find-first-basement-character [stair-number-list]
      (loop [stairs-remaining stair-number-list
             floor 0
             i 0]
        (cond (= floor -1) i
              (empty? stairs-remaining) 0
              :else (recur
                      (rest stairs-remaining)
                      (+ floor (first stairs-remaining))
                      (inc i)))))


(defn calc-first-basement-character [stair-list]
  (->> stair-list
       (map stair-to-number)
       (find-first-basement-character)))

(defn solve2 []
  (-> 1
      aoc/get-challenge
      get-floor-as-number
      calc-first-basement-character))

(defn -main [& args]
  (println (solve))
  (println (solve2)))

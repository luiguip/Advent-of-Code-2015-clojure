(ns day-3.core
  (:require [aoc-2015.core :as aoc]
            [clojure.string :as str])
  (:gen-class))

(def move-map {"^" [0 1] ">" [1 0] "v" [0 -1] "<" [-1 0]})

(defn add-move [coordinates raw-move]
  (let [move (get move-map raw-move)]
    (->> move
         (map + (last coordinates))
         (conj coordinates))))

(defn add-all-houses [raw-moves]
  (reduce add-move [[0 0]] raw-moves))


(defn add-all-houses-2 [raw-moves]
  (concat (add-all-houses (take-nth 2 raw-moves))
          (add-all-houses (take-nth 2 (rest raw-moves)))))

(defn solve []
  (-> (aoc/get-challenge 3)
      (str/split #"")
      add-all-houses
      set
      count))

(defn solve2 []
  (-> (aoc/get-challenge 3)
      (str/split #"")
      add-all-houses-2
      set
      count))


(defn -main
  [& args]
  (println (solve))
  (println (solve2)))

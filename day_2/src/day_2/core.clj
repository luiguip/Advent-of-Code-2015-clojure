(ns day-2.core
  (:require [aoc-2015.core :as aoc]
           [clojure.string :as str]))


(defn split-gifts [raw-text]
  (-> raw-text
    (str/split #"\n")))

(defn split-sides [gift]
  (->> (str/split gift #"x")
       (map #(Integer/parseInt %))))

(defn split-all-sides [gift-list]
  (->> gift-list
       (map split-sides)))

(defn comb-sides [sides]
  (->> (rest sides)
       (map #(* (first sides) %))))

(defn calc-wrap-paper-total [areas]
  (+ (apply + (concat areas areas))
     (apply min areas)))

(defn calc-wrap-paper [sides]
  (loop [areas []
         sides-remaining sides]
    (if (= (count sides-remaining) 1)
      (calc-wrap-paper-total areas)
      (recur (concat areas (comb-sides sides-remaining))
             (rest sides-remaining)))))

(defn calc-wrap-paper-all [sides-list]
  (->> sides-list
       (map calc-wrap-paper)
       (apply +)))

(defn calc-ribbon [sides]
  (loop [ribbon 0 sides-remaining (sort sides)]
    (if (= 1 (count sides-remaining))
           ribbon
           (let [head (first sides-remaining) tail (rest sides-remaining)]
             (recur (+ ribbon (* 2 head))
                    tail)))))

(defn calc-bow [sides]
  (apply * sides))

(defn calc-ribbon-and-bow [sides]
  (+ (calc-ribbon sides) (calc-bow sides)))

(defn calc-ribbon-and-bow-all [sides-list]
  (->> sides-list
       (map calc-ribbon-and-bow)
       (apply +)))

(defn solve []
  (-> (aoc/get-challenge 2)
      split-gifts
      split-all-sides
      calc-wrap-paper-all
      ))

(defn solve2 []
  (-> (aoc/get-challenge 2)
      split-gifts
      split-all-sides
      calc-ribbon-and-bow-all))

(defn -main [& args]
  (println (solve))
  (println (solve2)))
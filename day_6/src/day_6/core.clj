(ns day-6.core
  (:gen-class)
  (:require [aoc-2015.core :as aoc]
            [clojure.string :as str]))

(def do-map {"turn on" :on "turn off" :off "toggle" :toggle})

(defn init-matrix [x y val]
  (->> val
       (repeat x)
       vec
       (repeat y)
       vec))

(defn do-and-coordinates-to-map [do raw-coordinates]
  (let [coordinates (map #(Integer/parseInt %) raw-coordinates)
        xs [(first coordinates) (inc (nth coordinates 2))]
        ys [(nth coordinates 1) (inc (last coordinates))]]
    {:do (get do-map do)
     :coordinates {:xs xs :ys ys}}))

(defn raw-to-command-map [raw]
  (do-and-coordinates-to-map
    (first (re-seq #"^turn on|^turn off|^toggle" raw))
    (re-seq #"\d+" raw)))

(defn toggle [matrix [x y]]
  (update-in matrix [y x] #(bit-xor % 1)))

(defn turn [matrix x y value]
  (assoc-in matrix [y x] value))

(defn turn-on [matrix [x y]]
  (turn matrix x y 1))

(defn turn-off [matrix [x y]]
  (turn matrix x y 0))

(defn function-from-state [state]
  (case state
    :on turn-on
    :off turn-off
    :toggle toggle
    :else nil))

(defn generate-x-comb [xs ys]
  (let [x-range (apply range xs)
        y-range (apply range ys)]
    (->> x-range
         (repeat (count y-range))
         (apply interleave))))

(defn generate-y-comb [xs ys]
  (let [x-range (apply range xs)
        y-range (apply range ys)]
    (->> y-range
         (repeat (count x-range))
         flatten)))

(defn apply-function-matrix [function matrix coordinate-map]
  (let
    [xs (get coordinate-map :xs)
     ys (get coordinate-map :ys)
     x-comb (generate-x-comb xs ys)
     y-comb (generate-y-comb xs ys)]
    (->> (interleave x-comb y-comb)
         (partition 2 2)
         (reduce function matrix))))

(defn update-matrix [matrix do-coordinate-map]
  (let [state (get do-coordinate-map :do)
        coordinate-map (get do-coordinate-map :coordinates)]
    (-> (function-from-state state)
        (apply-function-matrix matrix coordinate-map))))

(defn update-all-matrix [matrix do-coordinate-map-vec]
  (reduce update-matrix matrix do-coordinate-map-vec))

(defn get-and-split []
  (-> (aoc/get-challenge 6)
      (str/split #"\n")))

(defn text-vec-to-data [text-vec]
  (map raw-to-command-map text-vec))

(defn count-lights-lit [matrix]
  (->> matrix
       flatten
       (apply +)))

(defn text-vec-to-final-matrix [matrix text-vec]
  (->> text-vec
       text-vec-to-data
       (update-all-matrix matrix)))

(defn solve []
  (let [matrix (init-matrix 1000 1000 0)]
    (->> (get-and-split)
         (text-vec-to-final-matrix matrix)
         (count-lights-lit))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (solve)))

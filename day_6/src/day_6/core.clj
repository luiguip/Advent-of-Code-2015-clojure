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

(defn map-of-coordinates [xs ys]
  (vec
    (map (fn [x y] {:x x :y y})
         (apply interleave (repeat (count xs) xs))
         (flatten (repeat (count ys) ys)))))

(defn generate-vec-from-range [xs]
  (range (first xs) (inc (last xs))))

(defn generate-map-of-coordinates-from-range [xs ys]
  (apply map-of-coordinates (map generate-vec-from-range [xs ys])))

(defn do-and-coordinates-to-map [do raw-coordinates]
  (let [coordinates (map #(Integer/parseInt %) raw-coordinates)
        xs [(first coordinates) (nth coordinates 1)]
        ys [(nth coordinates 2) (last coordinates)]]
    {:do (get do-map do)
     :coordinates (generate-map-of-coordinates-from-range xs ys)}))

(defn raw-to-command-map [raw]
  (do-and-coordinates-to-map
    (first (re-seq #"^turn on|^turn off|^toggle" raw))
    (re-seq #"\d+" raw)))

(defn toggle [matrix coordinate]
  (let [x (get coordinate :x)
        y (get coordinate :y)]
    (update-in matrix [y x] #(bit-xor % 1))))

(defn turn [matrix coordinate value]
  (let [x (get coordinate :x)
        y (get coordinate :y)]
    (assoc-in matrix [y x] value)))

(defn turn-on [matrix coordinate]
  (turn matrix coordinate 1))

(defn turn-off [matrix coordinate]
  (turn matrix coordinate 0))

(defn function-from-state [state]
  (case state
    :on turn-on
    :off turn-off
    :toggle toggle
    :else nil))

(defn update-matrix [matrix coordinate-map-vec]
  (let [state (get coordinate-map-vec :do)
        coordinate-vec (get coordinate-map-vec :coordinates)]
    (-> state
        function-from-state
        (reduce matrix coordinate-vec))))

(defn get-and-split []
  (-> (aoc/get-challenge 6)
      (str/split #"\n")))

(defn text-vec-to-data [text-vec]
  (map raw-to-command-map text-vec))

(defn solve []
  (let [matrix (init-matrix 1000 1000 0)]
  (->> (get-and-split)
      text-vec-to-data
       (reduce update-matrix matrix)
       (flatten)
       (apply +))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (solve)))

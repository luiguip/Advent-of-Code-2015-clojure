(ns day-5.core
  (:gen-class)
  (:require [aoc-2015.core :as aoc]))

(defn solve []
       (-> (aoc/get-challenge 5)))

(defn -main
  [& args]
  (println (solve)))

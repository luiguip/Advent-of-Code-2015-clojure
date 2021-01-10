(ns aoc-2015.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn set-file-name [x]
     (str "day" x ".txt"))

(defn get-challenge [x]
     (-> x
         set-file-name
         io/resource
         slurp))

(defn -main [& args]
  (println "aoc-2015"))
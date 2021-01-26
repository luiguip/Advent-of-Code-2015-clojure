(ns day-5.core
  (:gen-class)
  (:require [aoc-2015.core :as aoc]
            [clojure.string :as str]))

(defn at-least-3-vowels [word]
                   (<= 3
                       (->> word
                            (re-seq #"[aeiou]")
                            count)))

(defn a-double-letter [word]
  (some?
    (re-seq #"(\w)\1" word)))

(defn disallowed-words-free [word]
  (nil?
    (some true?
          (map #(str/includes? word %) ["ab" "cd" "pq" "xy"]))))

(defn pair-that-appears-at-least-two [word]
  (some?
    (re-seq #"(\w\w)\w*\1" word)))

(defn one-letter-between-same-letter [word]
  (some?
    (re-seq #"(\w)\w\1" word)))

(defn is-nice? [word]
  (and
    (at-least-3-vowels word)
    (a-double-letter word)
    (disallowed-words-free word)))

(defn is-nice2? [word]
  (and
    (pair-that-appears-at-least-two word)
    (one-letter-between-same-letter word)))

(defn filter-nice [word-list is-nice?]
  (filter is-nice? word-list))

(defn solve [is-nice?]
  (-> (aoc/get-challenge 5)
      (str/split #"\n")
      (filter-nice is-nice?)))

(defn save-solution []
  (spit "resources/solutionclj.txt" (str/join "\n" (solve))))

(defn -main
  [& args]
  (println (count (solve is-nice?)))
  (println (count (solve is-nice2?))))
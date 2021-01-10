(ns day-3.core-test
  (:require [clojure.test :refer :all]
            [day-3.core :refer :all]))

(def one-move-up [[0 0] [0 1]])
(def raw "^><<v")
(def moves ["^" ">" "<" "<" "v" "<" "<" "<"])
(def expected 2081)
(def expected2 2341)

(deftest add-move-test
  (testing "add-move-test"
    (is (= one-move-up (add-move [[0 0]] "^")))))

(deftest add-move-test2
  (testing "add-move-test2"
    (is (= [[0 0] [0 1] [0 2]] (add-move one-move-up "^")))))

(deftest  solve-test
  (testing "solve-test"
    (is (= expected (solve)))))

(deftest  solve-test-2
  (testing "solve-test"
    (is (= expected2 (solve2)))))

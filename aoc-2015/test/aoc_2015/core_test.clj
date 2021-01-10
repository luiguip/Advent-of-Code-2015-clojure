(ns aoc-2015.core_test
  (:require [clojure.test :refer :all]
            [aoc-2015.core :refer :all]))

(deftest set-file-name-test
  (testing "set-file-name test"
     (is (= (set-file-name  1) "day1.txt"))))
(ns day-1.core_test
  (:require [clojure.test :refer :all]
            [day-1.core :as core]))

(deftest find-first-empty-list
  (testing "Empty list find-first-basement-character"
    (is (= 0 (core/find-first-basement-character [])))))

(deftest find-first-character-floor
    (testing "Find the first character in the first floor"
     (is (= 1 (core/find-first-basement-character [-1])))))

(deftest find-first-character-test1
  (testing "Find frist character test 1"
    (is (= 5 (core/find-first-basement-character [1 -1 1 -1 -1])))))
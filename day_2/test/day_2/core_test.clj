(ns day-2.core-test
  (:require [clojure.test :refer :all]
            [day-2.core :refer :all]))

(def step-0 "1x1x1\n2x2x2\n1x2x3")
(def step-1 ["1x1x1" "2x2x2" "1x2x3"])
(def step-2 [["1" "1" "1"] ["2" "2" "2"] ["1" "2" "3"]])
(def step-3 [[1 1 1] [2 2 2] [1 2 3]])
(def step5 59)
(def step3-2 [1 2 3])
(def step5-2 6)
(def step6-2 6)
(def step7-2 12)
(def step8-2 33)
(def sp1 [2 3 4])
(def sp1r 10)

(deftest split-gifts-test
  (testing "split-gifts"
    (is (= step-1 (split-gifts step-0)))))

(deftest split-sides-test
  (testing "split-sides"
    (is (= (last step-3) (split-sides (last step-1))))))

(deftest split-all-sides-test
  (testing "split-all-sides"
    (is (= step-3 (split-all-sides step-1)))))

(deftest comb-sides-test
  (testing "bomb-sides-test"
    (is (= [6 8] (comb-sides [2 3 4])))))

(deftest calc-wrap-paper-test
  (testing "clac-wrap-paper"
    (is (= 58 (calc-wrap-paper [2 3 4])))))

(deftest calc-wrap-paper-all-test
  (testing "ccalc-wrap-paper-all-test"
    (is (= step5 (calc-wrap-paper-all step-3)))))

(deftest calc-ribbon-test
  (testing "calc-ribbon-test"
    (is (= step5-2 (calc-ribbon step3-2)))))

(deftest calc-ribbon-test2
  (testing "calc-ribbon-test2"
    (is (= sp1r (calc-ribbon sp1)))))

(deftest calc-bow-test
  (testing "calc-bow-test"
    (is (= step6-2 (calc-bow step3-2)))))

(deftest calc-ribbon-and-bow-test
  (testing "calc-ribbon-and-bow-test"
    (is (= step7-2 (calc-ribbon-and-bow step3-2)))))

(deftest calc-ribbon-and-bow-all-test
  (testing "calc-ribbon-and-bow-test"
    (is (= step8-2 (calc-ribbon-and-bow-all step-3)))))

(deftest remove-side-test
  (testing "calc-remove-side-test"
    (is (= (1 2 4 5) (remove-side [1 2 3 4 5] 2)))))
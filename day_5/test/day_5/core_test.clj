(ns day-5.core-test
  (:require [clojure.test :refer :all]
            [day-5.core :refer :all]))

(deftest at-least-3-vowels-cases
  (testing "at-least-3-vowels-testcases"
    (is (at-least-3-vowels "aei"))
    (is (false? (at-least-3-vowels "xyz")))))

(deftest double-letter-cases
  (testing "double-letter"
    (is (a-double-letter "xxyyzz"))
    (is (false? (a-double-letter "xyz")))
    (is (false? (a-double-letter "iylmzraibygmgmqj")))))

(deftest disallowed-words-cases
  (testing "disallowed-words-true"
    (is (disallowed-words-free "afb"))
    (is (false? (disallowed-words-free "ab")))))

(deftest filter-nice-true
  (testing "filter-nice"
    (is (= 4
           (count (filter-nice ["ab" "aaei" "xxa" "chrigojuzz" "ugknbfddgicrmopn" "aaa" "haegwjzuvuyypxyu
           'jchzalrnumimnmhp" "dvszwmarrgswjxmb" "iylmzraibygmgmqj"]))))))

(deftest pair-that-appears-at-least-two-cases
  (testing "pair-that-appears-at-least-two cases"
    (is (pair-that-appears-at-least-two "aabcdefgaa"))
    (is (false? (pair-that-appears-at-least-two "abcdefgaa")))))

(deftest one-letter-between-same-letter-cases
  (testing "one-letter-between-same-letter cases"
    (is (one-letter-between-same-letter "aya"))
    (is (one-letter-between-same-letter "aaa"))
    (is (false? (one-letter-between-same-letter "abcdefgaa")))))

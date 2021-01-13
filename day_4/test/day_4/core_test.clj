(ns day-4.core-test
  (:require [clojure.test :refer :all]
            [day-4.core :refer :all]))

(deftest md5-test
  (testing "md5 test"
    (is (= "49f68a5c8493ec2c0bf489821c21fc3b" (md5 "hi")))))

(deftest solve-test-1
  (testing "solve test"
    (is (= 346386 (solve "iwrupvqb" 5)))))

(deftest solve-test-1
  (testing "solve test"
    (is (= 9958218 (solve "iwrupvqb" 6)))))


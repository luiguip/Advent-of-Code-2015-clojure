(ns day-6.core-test
  (:require [clojure.test :refer :all]
            [day-6.core :refer :all]))

(def expected-map-vec [{:x 0 :y 0} {:x 1 :y 0} {:x 2 :y 0}
                       {:x 0 :y 1} {:x 1 :y 1} {:x 2 :y 1}
                       {:x 0 :y 2} {:x 1 :y 2} {:x 2 :y 2}])

(def map-vec-toggle [{:x 1 :y 0} {:x 0 :y 1}  {:x 0 :y 0} {:x 1 :y 1}])

(def command-str "turn on 0,2 through 0,2")

(def expected-matrix [[0 0 0] [0 0 0] [0 0 0]])

(def expected-matrix-toggle [[1 1 1] [1 1 1] [1 1 1]])

(def expected-final-matrix [[0 0 1] [0 0 1] [1 1 1]])

(def final-matrix-steps [{:do :on :coordinates expected-map-vec} {:do :toggle :coordinates map-vec-toggle}])

(deftest init-matrix-cases
  (testing "init-matrix tests"
    (is (= expected-matrix (init-matrix 3 3 0)))))

(deftest map-of-coordinates-cases
  (testing "map-of-coordinates tests"
    (is (= (set expected-map-vec) (set (map-of-coordinates [0 1 2] [0 1 2]))))))

(deftest generate-map-of-coordinates-from-range-cases
  (testing "generate-map-of-coordinates-from-range tests"
    (is (= (set expected-map-vec) (set (generate-map-of-coordinates-from-range [0 2] [0 2]))))))


(deftest do-and-coordinates-to-map-cases
  (testing "do-and-coordinates-to-map tests"
    (let [expected {:do :on :coordinates expected-map-vec}
          actual (do-and-coordinates-to-map "turn on" '("0" "2" "0" "2"))]
    (is (=
          (set (get  expected :coordinates))
          (set (get  actual :coordinates))))
    (is (=
          (get expected :do)
          (get actual :do))))))

(deftest raw-to-command-map-cases
  (testing "raw-to-command-map tests"
    (let [expected {:do :on :coordinates expected-map-vec}
          actual (raw-to-command-map command-str)]
      (is (=
            (set (get  expected :coordinates))
            (set (get  actual :coordinates))))
      (is (=
            (get expected :do)
            (get actual :do))))))

(deftest toggle-cases
  (testing "toggle tests"
    (is (= (update-in expected-matrix [0 0] #(bit-xor % 1)) (toggle expected-matrix {:x 0 :y 0})))
    (is (= expected-matrix-toggle (reduce toggle expected-matrix expected-map-vec)))))

(deftest update-matrix-cases
  (testing "update-matrix tests"
    (is (= expected-matrix-toggle (update-matrix expected-matrix {:do :toggle :coordinates expected-map-vec})))
    (is (= expected-matrix-toggle (update-matrix expected-matrix {:do :on :coordinates expected-map-vec})))
    (is (= expected-matrix (update-matrix expected-matrix-toggle {:do :off :coordinates expected-map-vec})))
    (is (= expected-final-matrix (reduce update-matrix expected-matrix-toggle final-matrix-steps)))))


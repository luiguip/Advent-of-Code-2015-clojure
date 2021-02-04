(ns day-6.core-test
  (:require [clojure.test :refer :all]
            [day-6.core :refer :all]))

(def expected-map-vec {:xs [0 3] :ys [0 3]})

(def map-vec-toggle {:xs [0 2] :ys [0 2]})

(def command-str "turn on 0,0 through 2,2")

(def expected-matrix [[0 0 0] [0 0 0] [0 0 0]])

(def expected-matrix-toggle [[1 1 1] [1 1 1] [1 1 1]])

(def expected-final-matrix [[0 0 1] [0 0 1] [1 1 1]])

(def final-matrix-steps [{:do :on :coordinates expected-map-vec} {:do :toggle :coordinates map-vec-toggle}])

(def matrix-count-lights-lit-0 (init-matrix 6 6 0))

(def matrix-count-lights-lit-1 [[0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 0 0]])

(def matrix-count-lights-lit-2 [[0 1 0 1 0 0] [0 1 0 1 0 0] [0 1 0 1 0 0] [0 1 0 1 0 0] [0 1 0 1 0 0] [0 1 0 1 0 0]])

(def matrix-count-lights-lit [[0 1 0 1 0 1] [0 1 0 1 0 1] [0 1 0 1 0 1] [0 1 0 1 0 1] [0 1 0 1 0 1] [0 1 0 1 0 1]])

(def count-lit 18)

(def matrix-count-lights-lit-text-vec ["turn on 1,0 through 1,5", "turn on 3,0 through 3,5", "toggle 5,0 through 5,5"])

(def matrix-count-lights-lit-steps [{:do :on :coordinates {:xs [1 2] :ys [0 6]}}
                                    {:do :on :coordinates {:xs [3 4] :ys [0 6]}}
                                    {:do :on :coordinates {:xs [5 6] :ys [0 6]}}])

(deftest init-matrix-cases
  (testing "init-matrix tests"
    (is (= expected-matrix (init-matrix 3 3 0)))))

(deftest do-and-coordinates-to-map-cases
  (testing "do-and-coordinates-to-map tests"
    (let [expected {:do :on :coordinates expected-map-vec}
          actual (do-and-coordinates-to-map "turn on" '("0" "0" "2" "2"))]
      (is (= (set (get  expected :coordinates)) (set (get  actual :coordinates))))
      (is (= (get expected :do) (get actual :do))))
    (let [expected {:do :on :coordinates {:xs [1 2] :ys [0 6]}}
          actual (do-and-coordinates-to-map "turn on" '("1" "0" "1" "5"))]
      (is (= (set (get expected :coordinates)) (set (get actual :coordinates))))
      (is (= (get expected :do) (get actual :do))))))

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
    (is (= (update-in expected-matrix [0 0] #(bit-xor % 1)) (toggle expected-matrix [0 0])))))

(deftest generate-x-comb-cases
  (testing "x-comb tests"
    (is (= '(0 0 0 1 1 1 2 2 2) (generate-x-comb [0 3] [0 3])))
    (is (= '(0 0 0 0 0) (generate-x-comb [0 1] [0 5])))
    (is (= '(0 1 2 3 4) (generate-x-comb [0 5] [0 1])))
    (is (= '(0 0 0 1 1 1) (generate-x-comb [0 2] [0 3])))))

(deftest generate-y-comb-cases
  (testing "y-comb tests"
    (is (= '(0 1 2 0 1 2 0 1 2) (generate-y-comb [0 3] [0 3])))
    (is (= '(0 1 2 3 4) (generate-y-comb [0 1] [0 5])))
    (is (= '(0 0 0 0 0) (generate-y-comb [0 5] [0 1])))
    (is (= '(0 1 2 0 1 2) (generate-y-comb [0 2] [0 3])))))

(deftest turn-all-cases
  (testing "turn-on turn-off and toggle tests"
    (is (= expected-matrix-toggle (apply-function-matrix toggle expected-matrix {:xs [0 3] :ys [0 3]})))
    (is (= expected-matrix-toggle (apply-function-matrix turn-on expected-matrix {:xs [0 3] :ys [0 3]})))
    (is (= expected-matrix (apply-function-matrix turn-off expected-matrix-toggle {:xs [0 3] :ys [0 3]})))
    (is (= matrix-count-lights-lit-1 (apply-function-matrix turn-on matrix-count-lights-lit-0 {:xs [1 2] :ys [0 6]})))
    (is (= matrix-count-lights-lit-2 (apply-function-matrix toggle matrix-count-lights-lit-1 {:xs [3 4] :ys [0 6]})))))

(deftest update-matrix-cases
  (testing "update-matrix tests"
    (is (= expected-matrix-toggle (update-matrix expected-matrix {:do :toggle :coordinates expected-map-vec})))
    (is (= expected-matrix-toggle (update-matrix expected-matrix {:do :on :coordinates expected-map-vec})))
    (is (= expected-matrix (update-matrix expected-matrix-toggle {:do :off :coordinates expected-map-vec})))))

(deftest update-all-matrix-cases
  (testing "udpate-all-matrix tests"
    (is (= expected-final-matrix (update-all-matrix expected-matrix final-matrix-steps)))
    (is (= matrix-count-lights-lit (update-all-matrix matrix-count-lights-lit-0 matrix-count-lights-lit-steps)))))

(deftest count-lights-lit-cases
  (testing "count-lights-lig tests"
    (is (= count-lit (count-lights-lit matrix-count-lights-lit)))))

(deftest text-vec-to-final-matrix-cases
  (testing "count-lights-lig tests"
    (is (= matrix-count-lights-lit-1 (text-vec-to-final-matrix (init-matrix 6 6 0) [(first matrix-count-lights-lit-text-vec)])))))

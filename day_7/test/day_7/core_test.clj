(ns day-7.core-test
  (:require [clojure.test :refer :all]
            [day-7.core :refer :all]))

(def vars {:vars {"a" 1 "b" 2}})
(def ops {:ops [{:op bit-and :x "a" :y "b" :r "c"} {:op bit-or :x "h" :y "gg" :r "aa"}]})
(def nots {:nots [{:op bit-not :x "a" :r "z"} {:op bit-not :x "h" :r "gg"}]})
(def general-map (merge vars ops nots))
(def raw-strings ["1 -> a", "h OR gg -> aa",
                  "NOT h -> gg", "NOT a -> z",
                  "a AND b -> c", "2 -> b"])
(def vars2-pre-merge '({"a" 1, "b" 2, "c" 0} {"a" 1, "b" 2} {"a" 1, "b" 2, "z" -2} {"a" 1, "b" 2}))
(def vars2 {:vars {"a" 1 "b" 2 "c" 0 "z" -2}})
(def ops2-2 {:ops [{:op bit-or :x "h" :y "gg" :r "aa"}]})
(def nots2-2 {:nots [{:op bit-not :x "h" :r "gg"}]})
(def general-map2 (merge vars2 ops nots))
(def general-map2-2 (merge vars2 ops2-2 nots2-2))
(def vars3 {:vars {"a" 1 "b" 2 "h" 10}})
(def vars3-1 {:vars {"a" 1 "b" 2 "h" 10 "gg" -11 "c" 0 "z" -2}})
(def general-map3 (merge vars3 ops nots))
(def general-map3-1 (merge vars3-1 {:ops [(get-in ops [:ops 1])]} {:nots []}))
(def vars3-2 {:vars {"a" 1 "b" 2 "h" 10 "gg" -11 "c" 0 "z" -2 "aa" -1}})

(deftest re-matches-initial-variable-cases
  (testing "re-matches-initial-variable tests"
    (is (= "0 -> a" (re-matches-initial-variable "0 -> a")))))

(deftest re-matches-not-cases
  (testing "re-matched-not tests"
    (is (= "NOT a -> b" (re-matches-not "NOT a -> b")))))

(deftest re-matches-operation-cases
  (testing "re-matches-operation tests"
    (is (= "a AND b -> c" (re-matches-operation "a AND b -> c")))))

(deftest set-intial-variable-cases
  (testing "set-initial tests"
    (is (= {"a"  0} (set-initial-variable "0 -> a")))))

(deftest set-not-map-cases
  (testing "set-not-map tests"
    (is (= {:op bit-not :x "a" :r "b"} (set-not-map "NOT a -> b")))))

(deftest case-logic-gate-cases
  (testing "case-logic-gate tests"
    (is (= bit-and (case-logic-gate "AND")))
    (is (= bit-or (case-logic-gate "OR")))
    (is (= bit-shift-left (case-logic-gate "LSHIFT")))
    (is (= bit-shift-right (case-logic-gate "RSHIFT")))))

(deftest set-operation-map-cases
  (testing "set-operation-map tests"
    (is (= {:op bit-and :x "a" :y "b" :r "c"} (set-operation-map "a AND b -> c")))
    (is (= {:op bit-or :x "a" :y "b" :r "c"} (set-operation-map "a OR b -> c")))
    (is (= {:op bit-shift-left :x "a" :y "b" :r "c"} (set-operation-map "a LSHIFT b -> c")))
    (is (= {:op bit-shift-right :x "a" :y "b" :r "c"} (set-operation-map "a RSHIFT b -> c")))))

(deftest op-map-from-string-cases
  (testing "op-map-from-string tests"
    (is (= [:vars {"a" 1}] (op-map-from-string "1 -> a")))
    (is (= [:ops [{:op bit-and :x "b" :y "c" :r "a"}]] (op-map-from-string "b AND c -> a")))
    (is (= [:nots [{:op bit-not :x "a" :r "b"}]] (op-map-from-string "NOT a -> b")))))

(deftest collection-from-key-test
  (is (= {} (collection-from-key :vars)))
  (is (= [] (collection-from-key :ops)))
  (is (= [] (collection-from-key :nots))))

(deftest add-op-map-to-nested-maps-cases
  (testing "add-op-map-to-nested-maps tests"
    (is (= {:vars {"a" 1}} (add-op-map-to-nested-maps {} [:vars {"a" 1}])))
    (is (= {:vars {"a" 1 "b" 2}} (add-op-map-to-nested-maps {:vars {"b" 2}} [:vars {"a" 1}])))
    (is (= {:ops [{:op bit-or :x "a" :y "b" :r "c"}]} (add-op-map-to-nested-maps {} [:ops [{:op bit-or :x "a" :y "b" :r "c"}]])))
    (is (= {:nots [{:op bit-not :x "a"  :r "c"}]} (add-op-map-to-nested-maps {} [:nots [{:op bit-not :x "a" :r "c"}]])))
    (is (= ops (add-op-map-to-nested-maps {:ops [{:op bit-and :x "a" :y "b" :r "c"}]}
                                          [:ops [{:op bit-or :x "h" :y "gg" :r "aa"}]])))
    (is (= nots (add-op-map-to-nested-maps {:nots [{:op bit-not :x "a" :r "z"}]}
                                           [:nots [{:op bit-not :x "h" :r "gg"}]])))))

(deftest raw-operation-to-map-cases
  (testing "raw-operation-to-map tests"
    (is (= {:ops [{:op bit-and :x "a" :y "b" :r "c"}]} (raw-operation-to-general-map {} "a AND b -> c")))
    (is (= {:ops [{:op bit-or :x "a" :y "b" :r "c"}]} (raw-operation-to-general-map {} "a OR b -> c")))
    (is (= {:ops [{:op bit-shift-left :x "a" :y "b" :r "c"}]} (raw-operation-to-general-map {} "a LSHIFT b -> c")))
    (is (= {:ops [{:op bit-shift-left :x "a" :y "b" :r "c"}]} (raw-operation-to-general-map {} "a LSHIFT b -> c")))
    (is (= {:nots [{:op bit-not :x "a" :r "b"}]} (raw-operation-to-general-map {} "NOT a -> b")))
    (is (= {nil []} (raw-operation-to-general-map {} "Invalid!")))
    (is (= {:vars {"a"  0}} (raw-operation-to-general-map {} "0 -> a")))))

(deftest raw-strings-to-general-map-cases
  (testing "raw-string-to-result tests"
    (let [result (raw-strings-to-general-map raw-strings)
          vars-result (:vars result)
          ops-result (:ops result)
          nots-result (:nots result)
          vars-expected (:vars general-map)
          ops-expected (:ops general-map)
          nots-expected (:nots general-map)]
      (is (= vars-expected vars-result))
      (is (= (set ops-expected) (set ops-result)))
      (is (= (set nots-expected) (set nots-result))))))

(deftest solve-op-cases
  (testing "solve-op tests"
    (is (= {"a" 1} (solve-op {"a" 1} {:op bit-and :x "a" :b "b" :r "c"})))
    (is (= {"a" 1 "b" 2 "c" 3} (solve-op {"a" 1 "b" 2} {:op bit-or :x "a" :y "b" :r "c"})))
    (is (= {"a" 1 "b" 2 "c" 4} (solve-op {"a" 1 "b" 2} {:op bit-shift-left :x "a" :y "b" :r "c"})))
    (is (= {"a" 1 "b" 2 "c" 0} (solve-op {"a" 1 "b" 2} {:op bit-shift-right :x "a" :y "b" :r "c"})))
    (is (= {"a" 1 "b" 2 "c" 0} (solve-op {"a" 1 "b" 2} {:op bit-and :x "a" :y "b" :r "c"})))))

(deftest solve-not-cases
  (testing "solve-not tests"
    (is (= {"a" 1 "b" -2} (solve-not {"a" 1} {:op bit-not :x "a" :r "b"})))))

(deftest contains-evalueated-op-cases
  (is (contains-evaluated-op? {"a" 1 "b" 2 "c" 3} {:op bit-or :x "a" :y "b" :r "c"}))
  (is (not (contains-evaluated-op? {"a" 1 "b" 2} {:op bit-or :x "a" :y "b" :r "c"})))
  (is (not (contains-evaluated-op? {"a" 1} {:op bit-or :x "a" :y "b" :r "c"})))
  (is (not (contains-evaluated-op? {"b" 2} {:op bit-or :x "a" :y "b" :r "c"}))))

(deftest contains-evalueated-not-cases
  (is (contains-evaluated-not? {"a" 1 "b" -2} {:op bit-not :x "a" :r "b"}))
  (is (not (contains-evaluated-not? {"a" 1} {:op bit-or :x "a" :r "b"})))
  (is (not (contains-evaluated-not? {"b" -2} {:op bit-or :x "a" :r "b"}))))

(deftest filter-evaluated-cases
  (is (= general-map2-2 (filter-evaluated general-map2))))

(deftest solve-all-test
  (is (= vars2-pre-merge (solve-all general-map))))

(deftest solve-iteration-test
  (is (= general-map2-2 (solve-iteration general-map)))
  (is (= general-map3-1 (solve-iteration general-map3))))

(deftest solve-iterations-test
  (is (= (get vars3-2 :vars) (solve-iterations general-map3))))


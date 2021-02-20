(ns day-8.core-test
  (:require [clojure.test :refer :all]
            [day-8.core :refer :all]))

(deftest calculate-test
  (is (= {:s "abab" :cns (repeat 2 escaped-cn)}
         (calculate escaped-re escaped-cn {:s "ab\\\"\\\"ab" :cns []})))
  (is (= {:s "abab" :cns (repeat 2 quotes-cn)}
         (calculate quotes-re quotes-cn {:s "\"abab\"" :cns []})))
  (is (= {:s "abab" :cns [hexa-cn]}
         (calculate hexa-re hexa-cn {:s "ab\\xabab" :cns []}))))

(deftest calculate-remain-test
  (is (= [escaped-cn hexa-cn {:ns 4 :ms 4}]
         (calculate-remain {:s "abab" :cns [escaped-cn hexa-cn]}))))

(deftest calculate-all-test
  (is (= {:ns 20 :ms 14} (calculate-all {:s "\"x\\\"\\xcaj\\xwwvpdldz\"" :cns []})))
  (is (= {:ns 2 :ms 0} (calculate-all {:s "\"\"" :cns []})))
  (is (= {:ns 10 :ms 7} (calculate-all {:s "\"aaa\\\"aaa\"" :cns []})))
  (is (= {:ns 14 :ms 7} (calculate-all {:s "\"a\\\\b\\\"a\\x32b\"" :cns []})))
  (is (= {:ns 6 :ms 1} (calculate-all {:s "\"\\x27\"" :cns []}))))

(deftest solve-test
  (is (= {:ns 40 :ms 22}
         (->> [{:s "\"\\x27\"" :cns []}
               {:s "\"a\\\\b\\\"a\\x32b\"" :cns []}
               {:s "\"x\\\"\\xcaj\\xwwvpdldz\"" :cns []}]
              (map calculate-all)
              (reduce sum-maps)))))

(deftest functional-tests
  (is (= {:ns 52 :ms 40}
         (->> (txt->str-list)
              (take 2)
              (map (fn [s] {:s s :cns []}))
              (map calculate-all)
              (reduce sum-maps)))))

(deftest final-test
  (testing "the file have 6202 chars! the memory count is wrong"
    (is (= {:ns 6202, :ms 4858} (solve1)))))

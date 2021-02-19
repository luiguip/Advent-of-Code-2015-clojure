(ns day-8.core-test
  (:require [clojure.test :refer :all]
            [day-8.core :refer :all]))

(deftest calculate-test
  (is (= {:s "abab" :cns (repeat 2 escaped-cn)}
         (calculate escaped-re escaped-cn {:s "ab\"\"ab" :cns []})))
  (is (= {:s "abab" :cns (repeat 2 quotes-cn)}
         (calculate quotes-re quotes-cn {:s "\"abab\"" :cns []})))
  (is (= {:s "abab" :cns [hexa-cn]}
         (calculate hexa-re hexa-cn {:s "ab\\xabab" :cns []}))))

(deftest calculate-remain-test
  (is (= [escaped-cn hexa-cn {:ns 4 :ms 4}]
         (calculate-remain {:s "abab" :cns [escaped-cn hexa-cn]}))))

(deftest calculate-all-test
  (is (= {:ns 14 :ms 7} (calculate-all {:s "\"a\\b\"a\\x32b\"" :cns []}))))

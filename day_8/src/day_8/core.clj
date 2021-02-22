(ns day-8.core
  (:require [clojure.string  :as str]
            [aoc-2015.core :as aoc])
  (:gen-class))

(def escaped-re #"\\[\"\\]")
(def escaped-cn {:ns 2 :ms 1})
(def hexa-re #"\\x[0-9a-fA-F]{2}")
(def hexa-cn {:ns 4 :ms 1})
(def quotes-re-first #"^\"")
(def quotes-re-last #"\"$")
(def quotes-cn {:ns 1 :ms 0})

(def escaped-decode-cn {:ns 4 :ms 2})
(def hexa-decode-cn {:ns 5 :ms 4})
(def quotes-decode-cn {:ns 2 :ms 1})

(defn calculate-quotes [cn {s :s cns :cns}]
  {:s (-> s
           (str/replace-first quotes-re-first "")
           (str/replace-first quotes-re-last ""))
    :cns (repeat 2 cn)})

(defn calculate [re cn m]
  (loop [{s :s cns :cns} m ]
    (if (nil? (re-find re s))
      {:s s :cns cns}
      (recur {:s (str/replace-first s re "")
              :cns (conj cns cn)}))))

(defn calculate-remain [{s :s cns :cns}]
  (->> s
      count
      ((fn [r] {:ns r :ms r}))
      (conj cns)))

(defn sum-maps [{tns :ns tms :ms} {ns :ns ms :ms}]
  {:ns (+ tns ns) :ms (+ tms ms)})

(defn calculate-all [m]
  (->> m
      (calculate-quotes quotes-cn)
      (calculate escaped-re escaped-cn)
      (calculate hexa-re hexa-cn)
      calculate-remain
      (reduce sum-maps)))

(defn calculate-all-2 [m]
  (->> m
       (calculate-quotes quotes-decode-cn)
       (calculate escaped-re escaped-decode-cn)
       (calculate hexa-re hexa-decode-cn)
       calculate-remain
       (reduce sum-maps)))

(defn txt->str-list []
  (->
   (aoc/get-challenge 8)
   (str/split #"\n")))

(defn solve1 []
  (->> (txt->str-list)
       (map (fn [s] {:s s :cns []}))
       (map calculate-all)
       (reduce sum-maps)))

(defn solve2 []
  (->> (txt->str-list)
       (map (fn [s] {:s s :cns []}))
       (map calculate-all-2)
       (reduce sum-maps)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

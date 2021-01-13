(ns day-4.core
  (:gen-class)
  (:require [clojure.string :as str] )
  (:import (java.security MessageDigest)))

(def input "iwrupvqb")

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn n-zeros [n]
  (let [zero-list (repeat n "0")]
    (str/join zero-list)))

(defn md5-starts-at-least-n-zeroes?
  [to-hash n]
  (let [hash (md5 to-hash)
        zeros (n-zeros n)]
      (str/starts-with? hash zeros)))

(defn solve
  ([number secret n]
   (loop [number number
          secret secret]
     (if (md5-starts-at-least-n-zeroes? (str secret number) n)
       number
       (recur (inc number) secret))))
  ([secret n]
   (solve 0 secret n)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (solve "iwrupvqb" 5))
  (println (solve "iwrupvqb" 6)))

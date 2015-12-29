(ns advent-of-code.day4
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

; returns the MD5 hash of a string s
; courtesy - https://gist.github.com/jizhang/4325757
(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn do-mining
  ([key] (do-mining key 5))
  ([key zero-count]
    (loop [i 0]
        (let [hash (md5 (str key i))
              zeros (take-while #(= % \0) hash)]
          (if (<= zero-count (count zeros)) i (recur (inc i)))))))

;; testcases
(assert (= (do-mining "abcdef") 609043))
(assert (= (do-mining "pqrstuv") 1048970))

;; first part
(do-mining "bgvyzdsv")

;; second part
(do-mining "bgvyzdsv" 6)

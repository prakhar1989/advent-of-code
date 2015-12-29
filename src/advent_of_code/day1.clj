(ns advent-of-code.day1
  (:require [clojure.java.io :as io]))

(defn count-steps [input]
  (->> input
       (map #(cond
              (= % \() 1
              (= % \)) -1))
       (reduce +)))

(def testcases
  ["(())" "()()"
   "(()(()(" "((("
   "))(((((" "())"
   "))(" ")))" ")())())"])

(doseq [t testcases]
  (do (println t (count-steps t))))

(def input (slurp (io/resource "day1-input.txt")))

(count-steps input)

;; part 2
(defn get-basement [input]
  (loop [i 0 total 0]
    (if (= -1 total) i
      (recur
        (inc i)
        (+ total (if (= (nth input i) \() 1 -1))))))


(get-basement input)

(ns advent-of-code.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn get-area [input]
  (let [[l b h] (->> (str/split input #"x")
                     (map #(Integer/parseInt %)))
        areas [(* l b) (* l h) (* b h)]
        slack (apply min areas)]
    (+ (->> areas
         (map #(* 2 %))
         (reduce +))
       slack)))

;; test case
(assert (= (get-area "2x3x4") 58))
(assert (= (get-area "1x1x10") 43))

(defn get-total-lines []
  (let [input (slurp (io/resource "day2-input.txt"))
        lines (str/split input #"\n")]
    (->>
      lines
      (map get-area)
      (reduce +))))

(get-total-lines)

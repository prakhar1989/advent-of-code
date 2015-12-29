(ns advent-of-code.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn has-vowels? [s]
  (<= 3 (->> (map #(#{\a \e \i \o \u} %) s)
               (filter (comp not nil?))
               (count))))

(defn has-repetition? [s]
  (< 0 (->> (partition 2 1 s)
            (filter (fn [[a b]] (= a b)))
            (count))))

(defn no-shady-chars? [s]
  (let [shady ["ab" "cd" "pq" "xy"]]
    (nil? (some #(.contains s %) shady))))

(defn is-nice-string? [s]
  (and (no-shady-chars? s)
       (has-repetition? s)
       (has-vowels? s)))

;; testcases
(assert (is-nice-string? "ugknbfddgicrmopn"))
(assert (is-nice-string? "aaa"))
(assert (not (is-nice-string? "jchzalrnumimnmhp")))
(assert (not (is-nice-string? "haegwjzuvuyypxyu")))
(assert (not (is-nice-string? "dvszwmarrgswjxmb")))

(defn get-total-lines []
  (let [input (slurp (io/resource "day5-input.txt"))
        lines (str/split input #"\n")]
    (count (filter is-nice-string? lines))))

(get-total-lines)

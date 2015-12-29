(ns advent-of-code.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util BitSet)))

(def size 1000)
(def grid-size (* size size))

(defn translate [[x y]]
  (+ (* x size) y))

(defn get-coords [[x1 y1] [x2 y2]]
  (for [x (range x1 (inc x2))
        y (range y1 (inc y2))]
    [x y]))

(defn light [bitset coord op]
  (let [i (translate coord)]
    (cond
      (= op :on) (.set bitset i)
      (= op :off) (.clear bitset i)
      (= op :toggle) (.flip bitset i))))

(defn parse [s]
  (let [parsed (re-seq #"(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)" s)
        [[_ op x1 y1 x2 y2]] parsed
        [x1 y1 x2 y2] (map #(Integer/parseInt %) [x1 y1 x2 y2])
        op (cond
             (= op "toggle") :toggle
             (= op "turn on") :on
             (= op "turn off") :off)]
    {:op op :from [x1 y1] :to [x2 y2]}))

(defn change-lights [s bitset]
  (let [{:keys [from to op]} (parse s)
        coords (get-coords from to)]
    (doseq [coord coords]
      (light bitset coord op))))

(defn run [input]
  (let [bitset (BitSet. grid-size)]
    (doseq [s input]
      (change-lights s bitset))
    (.cardinality bitset)))

(defn get-total-lights []
  (let [input (slurp (io/resource "day6-input.txt"))
        lines (str/split input #"\n")]
    (run lines)))

(get-total-lights)

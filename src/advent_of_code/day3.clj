(ns advent-of-code.day3
  (:require [clojure.java.io :as io]))

(def direction
  {\^ :north \v :south \> :east \< :west})

(defn move [[x y] dir]
  (cond
    (= dir :north) [(inc x) y]
    (= dir :south) [(dec x) y]
    (= dir :west)  [x (inc y)]
    (= dir :east)  [x (dec y)]))

(defn movein-grid [input]
  (loop [visited #{[0 0]}
         curr [0 0]
         dirs (map direction input)]
    (if (empty? dirs)
      ;(count visited)
      visited
      (recur
        (conj visited (move curr (first dirs)))
        (move curr (first dirs))
        (rest dirs)))))

;; testcases
(assert (= 2 (movein-grid "^v^v^v^v^v")))
(assert (= 2 (movein-grid ">")))
(assert (= 4 (movein-grid "^>v<")))

(def input (slurp (io/resource "day3-input.txt")))

;; answer for part 1
(count (movein-grid input))

;; part 2
(defn efficient-move [input]
  (let [santa-moves (take-nth 2 input)
        robo-moves (take-nth 2 (rest input))]
    (count (clojure.set/union
             (movein-grid robo-moves)
             (movein-grid santa-moves)))))

;; answer for part 2
(efficient-move input)


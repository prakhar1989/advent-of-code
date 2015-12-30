(ns advent-of-code.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn compute
  ([op x]
   (if (= op :NOT)
    (+ 65536 (bit-not x)) x))
  ([op x y]
   (cond
     (= op :AND)    (bit-and x y)
     (= op :OR)     (bit-or x y)
     (= op :LSHIFT) (bit-shift-left x y)
     (= op :RSHIFT) (bit-shift-right x y))))

(def RULE_PATTERNS
  {:set #"(\d+) -> (\w+)"
   :dual  #"(\w+) (AND|OR) (\w+) -> (\w+)"
   :assign #"(\w+) -> (\w+)"
   :shift #"(\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)"
   :unary #"NOT (\w+) -> (\w+)"})

(defn parse [s]
  (let [const-rule (re-seq (:set   RULE_PATTERNS) s)
        dual-rule  (re-seq (:dual  RULE_PATTERNS) s)
        shift-rule (re-seq (:shift RULE_PATTERNS) s)
        unary-rule (re-seq (:unary RULE_PATTERNS) s)
        assign-rule (re-seq (:assign RULE_PATTERNS) s)]
    (cond
      shift-rule
      (let [[[_ left op width right]] shift-rule]
        {:op (keyword op)
         :left [left (Integer/parseInt width)]
         :right right})
      const-rule
        (let [[[_ left right]] const-rule]
          {:op :SET
           :left (Integer/parseInt left)
           :right right})
      dual-rule
        (let [[[_ left1 op left2 right]] dual-rule]
          {:op (keyword op)
           :left [left1 left2]
           :right right})
      unary-rule
        (let [[[_ left right]] unary-rule]
          {:op :NOT
           :left left
           :right right})
      assign-rule
        (let [[[_ left right]] assign-rule]
          {:op :SET
           :left left
           :right right}))))

(defn build-tree [rules]
  (loop [tree {}
         rules rules]
    (if (empty? rules) tree
      (recur
        (let [rule (parse (first rules))]
          (assoc tree (:right rule) rule))
        (rest rules)))))

(defn evaluate [tree node]
  (if (not (tree node)) node
    (let [{:keys [op left]} (tree node)]
      (cond
        (= op :SET) (evaluate tree left)
        (= op :NOT) (compute :NOT (evaluate tree left))
        (or (= op :LSHIFT) (= op :RSHIFT))
          (compute op (evaluate tree (left 0)) (left 1))
        (or (= op :AND) (= op :OR))
          (compute op (evaluate tree (left 0)) (evaluate tree (left 1)))))))


;; testcases
(def rules
  ["123 -> x" "x OR y -> e" "x AND y -> d" "y -> n"
   "456 -> y" "x LSHIFT 2 -> f" "y RSHIFT 2 -> g"
   "NOT x -> h" "NOT y -> i" "x -> m"])

(def tree (build-tree rules))

(def testcases [["y" 456] ["h" 65412]
                ["x" 123] ["i" 65079]
                ["f" 492] ["g" 114]
                ["e" 507] ["d" 72]
                ["m" 123] ["n" 456]])

(doseq [[node value] testcases]
  (assert (= (evaluate tree node) value)))

;; part 1
(def rules (str/split (slurp (io/resource "day7-input.txt")) #"\n"))
(def part1-tree (build-tree rules))

(part1-tree "a")




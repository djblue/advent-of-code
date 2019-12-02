(ns advent-of-code.core-2019
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is are]]))

(defn get-input [file]
  (->> file io/resource slurp))

(defn file->vec [file]
  (read-string (str "[" (get-input file) "]")))

; https://adventofcode.com/2019/day/1

; --- Day 1: The Tyranny of the Rocket Equation ---

(defn fuel-required [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn total-requirements [f]
  (let [modules (file->vec "2019-day-01-input.txt")]
    (reduce + (map f modules))))

(deftest rocket-equation
  (are [mass fuel]
       (= (fuel-required mass) fuel)
    12 2
    14 2
    1969 654
    100756 33583)
  (is (total-requirements fuel-required) 3252897))

; https://adventofcode.com/2019/day/1#part2

; --- Part Two ---

(defn fuel-requirements+fuel [mass]
  (reduce + (take-while pos? (drop 1 (iterate fuel-required mass)))))

(deftest rocket-equation+fuel
  (are [mass fuel]
       (= (fuel-requirements+fuel mass) fuel)
    14 2
    1969 966
    100756 50346)
  (is (total-requirements fuel-requirements+fuel) 4876469))

; https://adventofcode.com/2019/day/2

; --- Day 2: 1202 Program Alarm ---

(defn intcode-vm
  ([opcodes] (intcode-vm opcodes opcodes))
  ([opcodes memory]
   (let [[op i j to & opcodes] opcodes]
     (if (= op 99)
       memory
       (let [a (get memory i)
             b (get memory j)
             f (case op 1 + 2 *)]
         (recur opcodes (assoc memory to (f a b))))))))

(defn run-program [opcodes a b]
  (-> opcodes (assoc 1 a) (assoc 2 b) intcode-vm first))

(deftest intcode-vm-examples
  (are [opcodes i out]
       (= (get (intcode-vm opcodes) i) out)
    [1 0 0 0 99] 0 2
    [2 3 0 3 99] 3 6
    [2 4 4 5 99 0] 5 9801
    [1 1 1 4 99 5 6 0 99] 4 2)
  (is (-> (file->vec "2019-day-02-input.txt")
          (run-program 12 2)
          (= 3790689))))

; --- Part Two ---

(defn find-inputs [opcodes out]
  (some
   (fn [[noun verb]]
     (when (= out (run-program opcodes noun verb))
       [noun verb]))
   (for [noun (range 100) verb (range 100)] [noun verb])))

(deftest intcode-vm-inputs-example
  (is
   (= (find-inputs (file->vec "2019-day-02-input.txt") 19690720)
      [65 33])))


(ns advent-of-code.core-2019
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is are]]))

(defn get-input [file]
  (->> file io/resource slurp))

; https://adventofcode.com/2019/day/1

; --- Day 1: The Tyranny of the Rocket Equation ---

(defn fuel-required [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn total-requirements [f]
  (let [input (get-input "2019-day-01-input.txt")
        modules (read-string (str "[" input "]"))]
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


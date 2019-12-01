(ns advent-of-code.core-2019
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is are]]))

(defn get-input [file]
  (->> file io/resource slurp))

; https://adventofcode.com/2019/day/1

; Day 1: The Tyranny of the Rocket Equation

(defn fuel-required [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn total-fuel-requirements []
  (let [input (get-input "2019-day-01-input.txt")
        modules (read-string (str "[" input "]"))]
    (reduce + (map fuel-required modules))))

(deftest rocket-equation
  (are [mass fuel]
       (= (fuel-required mass) fuel)
    12 2
    14 2
    1969 654
    100756 33583)
  (is (total-fuel-requirements) 3252897))


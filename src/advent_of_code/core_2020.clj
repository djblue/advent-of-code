(ns advent-of-code.core-2020
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

; https://adventofcode.com/2020/day/1

; --- Day 1: Report Repair ---

(defn expense-report-2 [numbers]
  (first
   (for [a numbers b numbers
         :when (= (+ a b) 2020)]
     (* a b))))

(defn expense-report-3 [numbers]
  (first
   (for [a numbers b numbers c numbers
         :when (= (+ a b c) 2020)]
     (* a b c))))

(def day-01-input
  (read-string
   (str "[" (-> "2020-day-01-input.txt" io/resource slurp) "]")))

(deftest expense-report-test
  (is (= 514579    (expense-report-2 [1721 979 366 299 675 1456])))
  (is (= 618144    (expense-report-2 day-01-input)))
  (is (= 241861950 (expense-report-3 [1721 979 366 299 675 1456])))
  (is (= 173538720 (expense-report-3 day-01-input))))

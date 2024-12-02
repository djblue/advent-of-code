(ns advent-of-code.core-2024
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

; --- Day 1: Historian Hysteria ---

(defn parse-input-day-1 [input]
  (let [numbers (->> (str/split input #"\s+")
                     (map parse-long)
                     (partition 2))]
    [(map first numbers)
     (map second numbers)]))

(defn day-1-solution [input]
  (let [[a b] (parse-input-day-1 input)]
    (reduce + (map (comp abs -) (sort a) (sort b)))))

(def input-01
  "3   4
4   3
2   5
1   3
3   9
3   3")

(defn day-1-solution-2 [input]
  (let [[a b] (parse-input-day-1 input)
        counts (frequencies b)]
    (reduce
     (fn [out n]
       (+ out (* n (get counts n 0))))
     0
     a)))

(deftest day-1
  (is (= 11 (day-1-solution input-01)))
  (is (= 2815556 (day-1-solution (slurp (io/resource "2024/01-input.txt")))))
  (is (= 31 (day-1-solution-2 input-01)))
  (is (= 23927637 (day-1-solution-2 (slurp (io/resource "2024/01-input.txt"))))))
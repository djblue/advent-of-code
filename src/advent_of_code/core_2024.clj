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

;; --- Day 2: Red-Nosed Reports ---

(def input-02
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse-input-day-2 [input]
  (->> (str/split-lines input)
       (map #(map parse-long (str/split % #"\s+")))))

(defn sign [n] (if (> n 0) 1 -1))

(defn all-increasing-or-decreasing [numbers]
  (not-any?
   (fn [[a b c]] (not= (sign (- a b)) (sign (- b c))))
   (partition 3 1 numbers)))

(defn at-least-one-at-most-three [numbers]
  (not-any?
   (fn [[a b]] (not (<= 1 (abs (- a b)) 3)))
   (partition 2 1 numbers)))

(defn safe-levels? [numbers]
  (and (all-increasing-or-decreasing numbers)
       (at-least-one-at-most-three numbers)))

(defn day-2-solution-1 [input]
  (let [input (parse-input-day-2 input)]
    (->> input
         (filter safe-levels?)
         (count))))

(defn all-but-one [numbers]
  (for [n (range (count numbers))]
    (keep-indexed
     (fn [idx item]
       (when (not= idx n)
         item))
     numbers)))

(defn safe-levels-2? [numbers]
  (some safe-levels? (all-but-one numbers)))

(defn day-2-solution-2 [input]
  (let [input (parse-input-day-2 input)]
    (->> input
         (filter safe-levels-2?)
         (count))))

(deftest day-2
  (is (= 2 (day-2-solution-1 input-02)))
  (is (= 631 (day-2-solution-1 (slurp (io/resource "2024/02-input.txt")))))
  (is (= 4 (day-2-solution-2 input-02)))
  (is (= 665 (day-2-solution-2 (slurp (io/resource "2024/02-input.txt"))))))
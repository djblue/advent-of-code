(ns advent-of-code.core
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

; --- Day 1: Chronal Calibration ---

; https://adventofcode.com/2018/day/1

(defn resulting-frequency
  ([ls] (resulting-frequency 0 ls))
  ([f ls]
   (cons
    f
    (if-let [delta (first ls)]
      (lazy-seq (resulting-frequency (+ f delta) (rest ls)))))))

(defn first-twice [ls]
  (loop [ls ls found #{}]
    (let [v (first ls)]
      (if (contains? found v)
        v
        (recur (rest ls) (conj found v))))))

(defn resource [name]
  (read-string (str "[" (slurp (io/resource name)) "]")))

; https://adventofcode.com/2018/day/1#part2

(deftest chronal-calibration
  (doseq [[in out]
          [[[+1 +1 +1] 3]
           [[+1 +1 -2] 0]
           [[-1 -2 -3] -6]
           [(resource "2018-day-01-input.txt") 472]]]
    (is (= (last (resulting-frequency in)) out)))
  (doseq [[in out]
          [[[+1 -1] 0]
           [[+3 +3 +4 -2 -4] 10]
           [[-6 +3 +8 +5 -6] 5]
           [[+7 +7 -2 -7 -4] 14]
           [(resource "2018-day-01-input.txt") 66932]]]
    (is (= (first-twice (resulting-frequency (cycle in))) out))))

(comment
  (clojure.test/run-tests *ns*))

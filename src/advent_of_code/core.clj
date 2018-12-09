(ns advent-of-code.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
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

; --- Day 2: Inventory Management System ---

; https://adventofcode.com/2018/day/2

(defn has-value? [n]
  (fn [ls] (some #(= % n) ls)))

(defn checksum [inventory]
  (let [counts (->> inventory (map frequencies) (map vals))]
    (* (->> counts (filter (has-value? 2)) count)
       (->> counts (filter (has-value? 3)) count))))

; https://adventofcode.com/2018/day/2#part2

(defn diff-string [a b]
  (loop [i 0 found? false result ""]
    (cond
      ; end of string
      (= i (count a))
      (if found? result nil)
      ; same character
      (= (get a i) (get b i))
      (recur (inc i) found? (str result (get a i)))
      ; different character
      found? nil
      :else (recur (inc i) true result))))

(defn common-letters [ls]
  (some identity (for [a ls b ls] (diff-string a b))))

(deftest inventory-management-system
  (doseq [[in out]
          [[["abcdef" "bababc"
             "abbcde" "abcccd"
             "aabcdd" "abcdee" "ababab"] 12]
           [(s/split (slurp (io/resource "2018-day-02-input.txt")) #"\n")
            7163]]]
    (is (= (checksum in) out)))
  (doseq [[in out]
          [[["abcde" "fghij"
             "klmno" "pqrst"
             "fguij" "axcye" "wvxyz"] "fgij"]
           [(s/split (slurp (io/resource "2018-day-02-input.txt")) #"\n")
            "ighfbyijnoumxjlxevacpwqtr"]]]
    (is (= (common-letters in) out))))

(comment
  (clojure.test/run-tests *ns*))

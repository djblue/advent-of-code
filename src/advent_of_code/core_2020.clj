(ns advent-of-code.core-2020
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

; --- Day 2: Password Philosophy ---

(defn valid-passwords-1 [s]
  (count
   (for [line (str/split-lines s)
         :let [[_ lowest highest [letter] password]
               (re-matches #"(\d+)-(\d+) (.): (.*)" line)
               password (frequencies password)
               lowest   (Integer/parseInt lowest)
               highest  (Integer/parseInt highest)]
         :when (<= lowest (get password letter 0) highest)]
     letter)))

(defn valid-passwords-2 [s]
  (count
   (for [line (str/split-lines s)
         :let [[_ i j [letter] password]
               (re-matches #"(\d+)-(\d+) (.): (.*)" line)
               i (dec (Integer/parseInt i))
               j (dec (Integer/parseInt j))]
         :when (not= (= (get password i) letter)
                     (= (get password j) letter))]
     letter)))

(def day-02-input (-> "2020-day-02-input.txt" io/resource slurp))

(deftest valid-passwords-test
  (is (= 2 (valid-passwords-1 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")))
  (is (= 519 (valid-passwords-1 day-02-input)))
  (is (= 1 (valid-passwords-2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")))
  (is (= 708 (valid-passwords-2 day-02-input))))

; --- Day 3: Toboggan Trajectory ---

(defn counting-trees [grid [right down]]
  (->> grid
       (keep-indexed
        (fn [index row]
          (when (zero? (mod index down)) row)))
       (map-indexed
        (fn [index row]
          (get row (mod (* index right) (count row)))))
       (filter #{\#})
       (count)))

(defn counting-trees-n [grid]
  (reduce
   (fn [n slope]
     (* n (counting-trees grid slope)))
   1
   [[1 1] [3 1] [5 1] [7 1] [1 2]]))

(def tree-grid
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(def day-03-input
  (-> "2020-day-03-input.txt" io/resource slurp str/split-lines))

(deftest counting-all-the-trees-test
  (is (= 7 (counting-trees tree-grid [3 1])))
  (is (= 189 (counting-trees day-03-input [3 1])))
  (is (= 336 (counting-trees-n tree-grid)))
  (is (= 1718180100 (counting-trees-n day-03-input))))

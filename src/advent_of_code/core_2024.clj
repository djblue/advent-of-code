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

;; --- Day 3: Mull It Over ---

(def input-03 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn day-3-solution-1 [input]
  (reduce
   +
   (for [[_ a b] (re-seq #"mul\((\d+),(\d+)\)" input)]
     (* (parse-long a) (parse-long b)))))

(def input-03-02 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn day-3-solution-2 [input]
  (:acc
   (reduce
    (fn [{:keys [state] :as out} [op a b]]
      (case op
        "don't()" (assoc out :state :dont)
        "do()" (assoc out :state :do)
        (if (= :dont state)
          out
          (update out :acc + (* (parse-long a) (parse-long b))))))
    {:acc 0 :state :do}
    (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" input))))

(deftest day-3
  (is (= 161 (day-3-solution-1 input-03)))
  (is (= 196826776 (day-3-solution-1 (slurp (io/resource "2024/03-input.txt")))))
  (is (= 48 (day-3-solution-2 input-03-02)))
  (is (= 106780429 (day-3-solution-2 (slurp (io/resource "2024/03-input.txt"))))))

;; --- Day 4: Ceres Search ---

(def input-04
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn day-4-solution-1-attempt-1 [input]
  (let [lines      (str/split-lines input)
        columns    (for [n (range (count lines))]
                     (str/join
                      (for [line lines] (nth line n))))
        diag-top-left
        (concat
         (for [n (butlast (range (count lines)))]
           (str/join
            (map-indexed
             (fn [index line]
               (nth line (- n index) ""))
             (reverse lines))))
         (for [n (range (count lines))]
           (str/join
            (map-indexed
             (fn [index line]
               (nth line (+ n index) ""))
             lines))))
        diag-top-right
        (concat
         (for [n (butlast (reverse (range (count lines))))]
           (str/join
            (map-indexed
             (fn [index line]
               (nth line (+ n index) ""))
             (reverse lines))))
         (for [n (reverse (range (count lines)))]
           (str/join
            (map-indexed
             (fn [index line]
               (nth line (- n index) ""))
             lines))))]
    (count
     (for [line  (concat lines columns diag-top-left diag-top-right)
           found (concat (re-seq #"XMAS" line)
                         (re-seq #"SAMX" line))]
       found))))

(defn x-mas [grid i j]
  (cond-> 0
   ;; row
    (or (and
         (= \X (get-in grid [(+ i 0) (+ j 0)]))
         (= \M (get-in grid [(+ i 0) (+ j 1)]))
         (= \A (get-in grid [(+ i 0) (+ j 2)]))
         (= \S (get-in grid [(+ i 0) (+ j 3)])))
        (and
         (= \S (get-in grid [(+ i 0) (+ j 0)]))
         (= \A (get-in grid [(+ i 0) (+ j 1)]))
         (= \M (get-in grid [(+ i 0) (+ j 2)]))
         (= \X (get-in grid [(+ i 0) (+ j 3)]))))
    inc

    ;; column
    (or (and
         (= \X (get-in grid [(+ i 0) (+ j 0)]))
         (= \M (get-in grid [(+ i 1) (+ j 0)]))
         (= \A (get-in grid [(+ i 2) (+ j 0)]))
         (= \S (get-in grid [(+ i 3) (+ j 0)])))
        (and
         (= \S (get-in grid [(+ i 0) (+ j 0)]))
         (= \A (get-in grid [(+ i 1) (+ j 0)]))
         (= \M (get-in grid [(+ i 2) (+ j 0)]))
         (= \X (get-in grid [(+ i 3) (+ j 0)]))))
    inc

    ;; diag 1
    (or (and
         (= \X (get-in grid [(+ i 0) (+ j 0)]))
         (= \M (get-in grid [(+ i 1) (+ j 1)]))
         (= \A (get-in grid [(+ i 2) (+ j 2)]))
         (= \S (get-in grid [(+ i 3) (+ j 3)])))
        (and
         (= \S (get-in grid [(+ i 0) (+ j 0)]))
         (= \A (get-in grid [(+ i 1) (+ j 1)]))
         (= \M (get-in grid [(+ i 2) (+ j 2)]))
         (= \X (get-in grid [(+ i 3) (+ j 3)]))))
    inc

    ;; diag 2
    (or (and
         (= \X (get-in grid [(- i 0) (+ j 0)]))
         (= \M (get-in grid [(- i 1) (+ j 1)]))
         (= \A (get-in grid [(- i 2) (+ j 2)]))
         (= \S (get-in grid [(- i 3) (+ j 3)])))
        (and
         (= \S (get-in grid [(- i 0) (+ j 0)]))
         (= \A (get-in grid [(- i 1) (+ j 1)]))
         (= \M (get-in grid [(- i 2) (+ j 2)]))
         (= \X (get-in grid [(- i 3) (+ j 3)]))))
    inc))

(defn day-4-solution-1 [input]
  (let [grid (mapv vec (str/split-lines input))]
    (reduce
     +
     (for [i (range (count grid))
           j (range (count grid))]
       (x-mas grid i j)))))

(defn xmas? [grid i j]
  (and
   (= \A (get-in grid [i j]))
   (or
    (and
     (= \M (get-in grid [(dec i) (dec j)]))
     (= \S (get-in grid [(inc i) (inc j)])))
    (and
     (= \S (get-in grid [(dec i) (dec j)]))
     (= \M (get-in grid [(inc i) (inc j)]))))
   (or
    (and
     (= \M (get-in grid [(dec i) (inc j)]))
     (= \S (get-in grid [(inc i) (dec j)])))
    (and
     (= \S (get-in grid [(dec i) (inc j)]))
     (= \M (get-in grid [(inc i) (dec j)]))))))

(defn day-4-solution-2 [input]
  (let [grid (mapv vec (str/split-lines input))]
    (count
     (for [i (range (count grid))
           j (range (count grid))
           :when (xmas? grid i j)]
       [i j]))))

(deftest day-4
  (is (= 18 (day-4-solution-1-attempt-1 input-04)))
  (is (= 2642 (time (day-4-solution-1-attempt-1 (slurp (io/resource "2024/04-input.txt"))))))
  (is (= 18 (day-4-solution-1 input-04)))
  (is (= 2642 (time (day-4-solution-1 (slurp (io/resource "2024/04-input.txt"))))))
  (is (= 9 (day-4-solution-2 input-04)))
  (is (= 1974 (day-4-solution-2 (slurp (io/resource "2024/04-input.txt"))))))
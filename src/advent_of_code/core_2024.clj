(ns advent-of-code.core-2024
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
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

;; --- Day 5: Print Queue ---

(def input-05
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse-input-day-5 [input]
  (let [[a b] (str/split input #"\n\n")]
    {:page-ordering-rules
     (reduce
      (fn [out line]
        (let [[_ a b] (re-matches #"(\d+)\|(\d+)" line)
              a (parse-long a) b (parse-long b)]
          (update out a (fnil conj #{}) b)))
      {}
      (str/split-lines a))
     :pages-to-produce
     (for [line (str/split-lines b)]
       (map parse-long (str/split line #",")))}))

(defn correctly-ordered [{:keys [page-ordering-rules]} page-order]
  (sort
   (fn [a b]
     (cond
       (get-in page-ordering-rules [a b]) -1
       (get-in page-ordering-rules [b a]) 1
       :else      0))
   page-order))

(defn day-5-solution-1 [input]
  (let [{:keys [pages-to-produce] :as in} (parse-input-day-5 input)]
    (reduce
     +
     (keep
      (fn [page-order]
        (when (= page-order (correctly-ordered in page-order))
          (nth page-order (quot (count page-order) 2))))
      pages-to-produce))))

(defn day-5-solution-2 [input]
  (let [{:keys [pages-to-produce] :as in} (parse-input-day-5 input)]
    (reduce
     +
     (keep
      (fn [page-order]
        (let [page-order' (correctly-ordered in page-order)]
          (when-not (= page-order' page-order)
            (nth page-order' (quot (count page-order') 2)))))
      pages-to-produce))))

(deftest day-5
  (is (= 143 (day-5-solution-1 input-05)))
  (is (= 5948 (day-5-solution-1 (slurp (io/resource "2024/05-input.txt")))))
  (is (= 123 (day-5-solution-2 input-05)))
  (is (= 3062 (day-5-solution-2 (slurp (io/resource "2024/05-input.txt"))))))

;; --- Day 6: Guard Gallivant ---

(def input-06
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn parse-input-day-6 [input]
  (let [grid (mapv vec (str/split-lines input))
        start-position (some
                        (fn [row]
                          (some
                           (fn [column]
                             (when (= \^ (get-in grid [row column]))
                               {:direction :up
                                :row row
                                :column column}))
                           (range (count (get grid row)))))
                        (range (count grid)))]
    {:grid grid
     :guard start-position
     :visited #{(select-keys start-position [:row :column])}
     :states #{start-position}}))

(defn patrol-protocol-step [{grid :grid states :states {:keys [direction row column]} :guard :as input}]
  (let [next-position (case direction
                        :up    {:row (dec row) :column column}
                        :down  {:row (inc row) :column column}
                        :left  {:row row :column (dec column)}
                        :right {:row row :column (inc column)})
        next-block (get-in grid ((juxt :row :column) next-position))
        next-state (assoc next-position :direction direction)]
    (if (contains? states next-state)
      :loop
      (case next-block
        (\^ \.) (recur
                 (-> input
                     (update :visited conj next-position)
                     (update :states conj next-state)
                     (update :guard merge next-position)))
        \# (recur
            (update input :guard
                    assoc :direction
                    (case direction
                      :up :right
                      :right :down
                      :down :left
                      :left :up)))
        nil input))))

(defn patrol-protocol [input]
  (count (:visited (patrol-protocol-step (parse-input-day-6 input)))))

(defn patrol-protocol-looper [input]
  (let [{:keys [grid] :as in} (parse-input-day-6 input)
        {:keys [visited]} (patrol-protocol-step in)]
    (count
     (for [row    (range (count grid))
           column (range (count (get grid row)))
           :when  (contains? visited {:row row :column column})
           :when  (not (#{\^ \#} (get-in grid [row column])))
           :when  (= :loop (patrol-protocol-step (assoc in :grid (assoc-in grid [row column] \#))))]
       {:row row :column column}))))

(deftest day-6
  (is (= 41 (patrol-protocol input-06)))
  (is (= 5153 (patrol-protocol (slurp (io/resource "2024/06-input.txt")))))
  (is (= 6 (patrol-protocol-looper input-06)))
  (comment (is (= 1711 (patrol-protocol-looper (slurp (io/resource "2024/06-input.txt")))))))

;; --- Day 7: Bridge Repair ---

(def input-07
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input-day-7 [input]
  (for [line (str/split-lines input)
        :let [[total numbers] (str/split line #": ")]]
    {:total   (parse-long total)
     :numbers (long-array (mapv parse-long (str/split numbers #" ")))}))

(defn scale ^long [n]
  (loop [^long n (abs n) m 1]
    (let [x (quot n 10)]
      (if (> 1 x)
        m
        (recur x (* m 10))))))

(defn || [^long a ^long b] (+ (* 10 a (scale b)) b))

(defn day-7-solution [part input]
  (let [ops (case part 1 [0 1] 2 [0 1 2])
        selections (memoize (comp #(map object-array %) combo/selections))]
    (->> (parse-input-day-7 input)
         (keep
          (fn [{:keys [^long total ^longs numbers]}]
            (let [n (alength numbers)]
              (some
               (fn [^objects combo]
                 (when (== total
                           (loop [^long a (first numbers) i 0]
                             (let [j (inc i)]
                               (cond
                                 (> a total) -1
                                 (>= j n)    a
                                 :else
                                 (recur
                                  (let [op (aget combo i) b (aget numbers j)]
                                    (cond
                                      (== op 0) (+ a b)
                                      (== op 1) (* a b)
                                      :else     (|| a b))) j)))))
                   total))
               (selections ops (dec (count numbers)))))))
         (reduce +))))

(deftest day-7
  (is (= 3749 (day-7-solution 1 input-07)))
  (is (= 538191549061 (day-7-solution 1 (slurp (io/resource "2024/07-input.txt")))))
  (is (= 11387 (day-7-solution 2 input-07)))
  (is (= 34612812972206 (day-7-solution 2 (slurp (io/resource "2024/07-input.txt"))))))

;; --- Day 8: Resonant Collinearity ---

(def input-08
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn parse-input-day-8 [input]
  (let [grid (mapv vec (str/split-lines input))
        rows (count grid)
        columns (count (first grid))]
    {:grid grid
     :rows rows
     :columns columns
     :antennas
     (for [row    (range rows)
           column (range columns)
           :let [value (get-in grid [row column])]
           :when (not= \. value)]
       {:row row
        :column column
        :value value})}))

(defn day-8-solution-1 [input]
  (let [{:keys [rows columns antennas]} (parse-input-day-8 input)]
    (reduce
     (fn [out {:keys [value row column]}]
       (conj out {:row row :column column :value value}))
     #{}
     (for [[_node locations] (group-by :value antennas)
           [a b] (combo/combinations locations 2)
           [a b] [[a b] [b a]]
           :let [row    (+ (:row b) (- (:row b) (:row a)))
                 column (+ (:column b) (- (:column b) (:column a)))]
           :when (and (< -1 row rows) (< -1 column columns))]
       {:value \#
        :row row
        :column column}))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn day-8-solution-2 [input]
  (let [{:keys [rows columns antennas]} (parse-input-day-8 input)]
    (reduce
     (fn [out {:keys [value row column]}]
       (conj out {:row row :column column :value value}))
     #{}
     (for [[_node locations] (group-by :value antennas)
           [a b] (combo/combinations locations 2)
           :let [row     (- (:row b) (:row a))
                 column  (- (:column b) (:column a))
                 scale   (gcd row column)
                 row     (quot row scale)
                 column  (quot column scale)
                 n       (max rows columns)]
           n (range (- n) n 1)
           :let [row (+ (:row b) (* n row))
                 column (+ (:column b) (* n column))]
           :when (and (< -1 row rows)
                      (< -1 column columns))]
       {:value \#
        :row row
        :column column}))))

(deftest  day-8
  (is (= 14 (count (day-8-solution-1 input-08))))
  (is (= 291 (count (day-8-solution-1 (slurp (io/resource "2024/08-input.txt"))))))
  (is (= 34 (count (day-8-solution-2 input-08))))
  (is (= 1015 (count (day-8-solution-2 (slurp (io/resource "2024/08-input.txt")))))))
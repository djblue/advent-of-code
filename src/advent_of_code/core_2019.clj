(ns advent-of-code.core-2019
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [intersection]]
            [clojure.test :refer [deftest is are]]))

(defn get-input [file]
  (->> file io/resource slurp))

(defn file->vec [file]
  (read-string (str "[" (get-input file) "]")))

; https://adventofcode.com/2019/day/1

; --- Day 1: The Tyranny of the Rocket Equation ---

(defn fuel-required [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn total-requirements [f]
  (let [modules (file->vec "2019-day-01-input.txt")]
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

; https://adventofcode.com/2019/day/2

; --- Day 2: 1202 Program Alarm ---

(defn intcode-vm
  ([opcodes] (intcode-vm opcodes opcodes))
  ([opcodes memory]
   (let [[op i j to & opcodes] opcodes]
     (if (= op 99)
       memory
       (let [a (get memory i)
             b (get memory j)
             f (case op 1 + 2 *)]
         (recur opcodes (assoc memory to (f a b))))))))

(defn run-program [opcodes a b]
  (-> opcodes (assoc 1 a) (assoc 2 b) intcode-vm first))

(deftest intcode-vm-examples
  (are [opcodes i out]
       (= (get (intcode-vm opcodes) i) out)
    [1 0 0 0 99] 0 2
    [2 3 0 3 99] 3 6
    [2 4 4 5 99 0] 5 9801
    [1 1 1 4 99 5 6 0 99] 4 2)
  (is (-> (file->vec "2019-day-02-input.txt")
          (run-program 12 2)
          (= 3790689))))

; --- Part Two ---

(defn find-inputs [opcodes out]
  (some
   (fn [[noun verb]]
     (when (= out (run-program opcodes noun verb))
       [noun verb]))
   (for [noun (range 100) verb (range 100)] [noun verb])))

(deftest intcode-vm-inputs-example
  (is
   (= (find-inputs (file->vec "2019-day-02-input.txt") 19690720)
      [65 33])))

; https://adventofcode.com/2019/day/3

(defn interop [point op]
  (let [[x y] point
        [direction n] op]
    (case direction
      \R [(range x (+ x n 1) +1) (repeat y)]
      \L [(range x (- x n 1) -1) (repeat y)]
      \U [(repeat x) (range y (+ y n 1) +1)]
      \D [(repeat x) (range y (- y n 1) -1)])))

(defn line-points [line]
  (loop [point [0 0]
         step 0
         [op & line] line
         points {}]
    (if (nil? op)
      points
      (let [[_ n] op
            ks (apply map vector (interop point op))]
        (recur (last ks)
               (+ step n)
               line
               (merge-with (fn [a b] a)
                           points
                           (zipmap ks
                                   (range step (+ step n 1)))))))))

(defn parse-lines [s]
  (map #(let [[direction & n] %]
          [direction (read-string (apply str n))]) s))

(defn find-intersection [a b]
  (let [m1 (-> a parse-lines line-points (dissoc [0 0]))
        m2 (-> b parse-lines line-points (dissoc [0 0]))
        s1 (-> m1 keys set) s2 (-> m2 keys set)]
    [m1 m2 (intersection s1 s2)]))

(defn find-min-manhattan [a b]
  (let [[m1 m2 points] (find-intersection a b)]
    (->> points
         (filter (fn [[x y]] (and (> x 0) (> y 0))))
         (map (fn [[x y]] (+ x y)))
         (apply min))))

(defn find-min-step [a b]
  (let [[m1 m2 points] (find-intersection a b)]
    (->> points
         (map (fn [point]
                (let [p1 (get m1 point)
                      p2 (get m2 point)]
                  (+ p1 p2))))
         (apply min))))

(deftest crossed-wires
  (are [paths min-manhattan min-step]
       (let [[p1 p2] paths]
         (is (= (find-min-manhattan p1 p2) min-manhattan))
         (is (= (find-min-step p1 p2) min-step)))

    [["R8" "U5" "L5" "D3"]
     ["U7" "R6" "D4" "L4"]]
    6
    30

    [["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
     ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]]
    159
    610

    [["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
     ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"]]
    135
    410

    (let [file (slurp (io/resource "2019-day-03-input.txt"))]
      (->> (s/split file #"\n") (map #(s/split % #","))))
    1674
    14012))


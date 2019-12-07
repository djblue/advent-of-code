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

(defn intcode-vm [memory]
  (loop [[op i j to & opcodes] memory
         memory memory]
    (if (= op 99)
      memory
      (let [a (get memory i)
            b (get memory j)
            f ({1 + 2 *} op)]
        (recur opcodes (assoc memory to (f a b)))))))

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

; https://adventofcode.com/2019/day/4

; --- Day 4: Secure Container ---

(defn digits [x]
  (if (< x 10)
    [x]
    (conj (digits (quot x 10)) (rem x 10))))

(defn at-least-two-adjacent? [n]
  (some (fn [[a b]] (= a b)) (partition 2 1 n)))

(defn exactly-two-adjacent? [n]
  (some #(= % 2) (vals (frequencies n))))

(defn never-decrease-digits? [n]
  (= (sort n) n))

(defn different-passwords [f a b]
  (->> (range a b)
       (map digits)
       (filter never-decrease-digits?)
       (filter f)
       count))

(deftest secure-container
  (is (at-least-two-adjacent? (digits 122345)))
  (is (never-decrease-digits? (digits 111123)))
  (is (exactly-two-adjacent? (digits 112233)))
  (is (not (exactly-two-adjacent? (digits 123444))))
  (is (exactly-two-adjacent? (digits 111122)))
  (is (= (different-passwords at-least-two-adjacent? 138241 674034) 1890))
  (is (= (different-passwords exactly-two-adjacent? 138241 674034) 1277)))

; --- Day 5: Sunny with a Chance of Asteroids ---

(defn parse-opcode [op]
  (let [[a b c d e]
        (->> op
             digits
             (concat (repeat 5 0))
             (take-last 5))]
    [(+ (* 10 d) e) c b a]))

(defn memory-read [addr mode memory]
  (if (= mode 1)
    addr
    (get memory addr)))

(def op->fn {1 + 2 *
             5 not= 6 =
             7 #(if (< %1 %2) 1 0)
             8 #(if (= %1 %2) 1 0)})

(defn intcode-vm-new [state]
  (let [{:keys [pc in out memory] :or {pc 0}} state
        [op & args] (subvec memory pc)
        [op & modes] (parse-opcode op)
        f (op->fn op)
        pc (inc pc)]
    (if (= op 99) ; halt
      state
      (recur
       (merge
        state
        (case op
          ; arithmetic / less than / equals
          (1 2 7 8)
          (let [[a b c] args
                [m1 m2] modes
                a (memory-read a m1 memory)
                b (memory-read b m2 memory)
                memory (assoc memory c (f a b))]
            {:memory memory :pc (+ pc 3)})

          ; read input
          3 (let [[value & in] in
                  [addr] args
                  memory (assoc memory addr value)]
              {:in in :memory memory :pc (inc pc)})

          ; write output
          4 (let [[value] args
                  [mode] modes
                  value (memory-read value mode memory)
                  out (conj out value)]
              {:out out :memory memory :pc (inc pc)})

          ; jump-if-true / jump-if-false
          (5 6)
          (let [[value dest] args
                [m1 m2] modes
                value (memory-read value m1 memory)
                dest (memory-read dest m2 memory)]
            {:pc (if (f value 0) dest (+ pc 2))})))))))

(defn run-program-new [program in]
  (-> {:memory program :in in} intcode-vm-new :out first))

(deftest sunny-with-a-chance-of-asteroids
  ; read stdin
  (is (-> {:memory [3 0 99] :in [:hello]}
          intcode-vm-new
          :memory
          first
          (= :hello)))

  ; write stdout
  (is (-> {:memory [4 0 99]}
          intcode-vm-new
          :out
          first
          (= 4)))

  ; in -> out
  (is (-> {:memory [3 0 4 0 99] :in [:hello]}
          intcode-vm-new
          :out
          first
          (= :hello)))

  (is (-> {:memory [1002 4 3 4 33]}
          intcode-vm-new
          :memory
          last
          (= 99)))

  (are [in out]
       (is (= (run-program-new (file->vec "2019-day-05-input.txt") [in]) out))
    1 16574641
    5 15163975)

  (are [in out]
       (is (= out
              (run-program-new [3 3 1108 -1 8 3 4 3 99] [in])
              (run-program-new [3 9 8 9 10 9 4 9 99 -1 8] [in])))
    7 0
    8 1
    9 0)

  (are [in out]
       (is (= out
              (run-program-new [3 3 1107 -1 8 3 4 3 99] [in])
              (run-program-new [3 9 7 9 10 9 4 9 99 -1 8] [in])))
    7 1
    8 0
    9 0)

  (are [in out]
       (is (= out
              (run-program-new [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [in])
              (run-program-new [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [in])))
    -1 1
    0 0
    1 1)

  (are [in out]
       (is (= out
              (run-program-new [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] [in])))
    6 999
    7 999
    8 1000
    9 1001
    10 1001))


; --- Day 6: Universal Orbit Map ---

(defn read-orbits []
  (->> (s/split (get-input "2019-day-06-input.txt") #"\n")
       (map #(s/split % #"\W"))
       (reduce (fn [m [v k]] (assoc m k v)) {})))

(defn object-orbits [object orbits]
  (loop [path [] object object]
    (if-let [orbiting (orbits object)]
      (recur (conj path orbiting) orbiting)
      path)))

(defn total-orbits []
  (let [orbits (read-orbits)]
    (->>  orbits
          (map #(object-orbits (first %) orbits))
          (map count)
          (reduce +))))

(defn orbits-between [a b]
  (let [p1 (object-orbits a (read-orbits))
        p2 (object-orbits b (read-orbits))
        common (some (set p1) p2)
        p1 (take-while #(not= common %) p1)
        p2 (take-while #(not= common %) p2)]
    (+ (count p1) (count p2))))

(deftest universal-orbit-map
  (is (= (total-orbits) 150150))
  (is (= (orbits-between "YOU" "SAN")
         (orbits-between "SAN" "YOU")
         352)))


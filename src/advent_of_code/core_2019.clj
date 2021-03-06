(ns advent-of-code.core-2019
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [intersection]]
            [clojure.test :refer [deftest is are]]
            [clojure.core.async :as a :refer [>! <! <!!]]
            [clojure.math.combinatorics :as combo]
            [criterium.core :as c]))

(defn get-input [file]
  (->> file io/resource slurp))

(defn file->vec [file]
  (read-string (str "[" (get-input file) "]")))

; https://adventofcode.com/2019/day/1

; --- Day 1: The Tyranny of the Rocket Equation ---

(defn fuel-required [mass] (- (quot mass 3) 2))

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
  (reduce + (take-while pos? (rest (iterate fuel-required mass)))))

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

(defn resolve-addr [addr mode state]
  (case mode
    2 (+ (get state :relative-base 0) addr)
    addr))

(defn memory-read [addr mode state]
  (case mode
    1 addr
    (get-in state [:memory (resolve-addr addr mode state)] 0)))

(def op->fn {1 +' 2 *'
             5 not= 6 =
             7 #(if (< %1 %2) 1 0)
             8 #(if (= %1 %2) 1 0)})

(defn vec->map [v]
  (into {} (map-indexed #(-> [%1 %2]) v)))

(defn intcode-vm-new-async [state in out]
  (a/go-loop [state (update state :memory vec->map)]
    (let [{:keys [pc memory relative-base]
           :or {pc 0 relative-base 0}}
          state
          [op & args] [(get memory pc)
                       (get memory (+ 1 pc) 0)
                       (get memory (+ 2 pc) 0)
                       (get memory (+ 3 pc) 0)]
          [op & modes] (parse-opcode op)
          f (op->fn op)
          pc (inc pc)]
      (if (= op 99) ; halt
        (do (a/close! out) state)
        (recur
         (merge
          state
          (case op
            ; arithmetic / less than / equals
            (1 2 7 8)
            (let [[a b c] args
                  [m1 m2 m3] modes
                  a (memory-read a m1 state)
                  b (memory-read b m2 state)
                  c (resolve-addr c m3 state)
                  memory (assoc memory c (f a b))]
              {:memory memory :pc (+ pc 3)})

            ; read input
            3
            (let [value (<! in)
                  [addr] args
                  addr (resolve-addr addr (first modes) state)
                  memory (assoc memory addr value)]
              {:memory memory :pc (inc pc)})

            ; write output
            4
            (let [[addr] args
                  [mode] modes
                  value (memory-read addr mode state)]
              (>! out value)
              {:memory memory :pc (inc pc)})

            ; jump-if-true / jump-if-false
            (5 6)
            (let [[value dest] args
                  [m1 m2] modes
                  value (memory-read value m1 state)
                  dest (memory-read dest m2 state)]
              {:pc (if (f value 0) dest (+ pc 2))})

            ; adjusts the relative base
            9
            (let [[addr] args
                  [mode] modes
                  value (memory-read addr mode state)]
              {:relative-base (+ relative-base value) :pc (inc pc)}))))))))

(defn intcode-vm-new [state]
  (let [in (a/to-chan (:in state)) out (a/chan (a/sliding-buffer 100))
        [state] (a/alts!! [(intcode-vm-new-async state in out)
                           (a/timeout 5000)])]
    (a/close! out)
    (when state
      (assoc state :out (or (<!! (a/into '() out)) [])))))

(defn run-program-new [program in]
  (-> {:memory program :in in} intcode-vm-new :out first))

(deftest sunny-with-a-chance-of-asteroids
  ; read stdin
  (is (-> {:memory [3 0 99] :in [:hello]}
          intcode-vm-new
          :memory
          (get 0)
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
          (get 4)
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

; --- Day 7: Amplification Circuit ---

(defn try-phase-settings [program settings]
  (loop [[phase & settings] settings input 0]
    (if-not phase
      input
      (recur
       settings
       (run-program-new program [phase input])))))

(defn find-max-thrust [program]
  (->> (combo/permutations [0 1 2 3 4])
       (map #(try-phase-settings program %))
       (apply max)))

(deftest amplification-circuit
  (are [program settings max]
       (= (try-phase-settings program settings)
          (find-max-thrust program)
          max)
    [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0]
    [4 3 2 1 0]
    43210

    [3 23 3 24 1002 24 10 24 1002 23 -1 23
     101 5 23 23 1 24 23 23 4 23 99 0 0]
    [0 1 2 3 4]
    54321

    [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
     1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0]
    [1 0 4 3 2]
    65210)

  (is (= (find-max-thrust (file->vec "2019-day-07-input.txt"))
         43812)))

(defn run-program-async [program setting]
  (let [in (a/chan) out (a/chan)
        state (intcode-vm-new-async {:memory program} in out)]
    (a/put! in setting)
    {:in in :out out :state state}))

(defn try-phase-settings-with-feedback [program settings]
  (let [[a b c d e] settings
        a (run-program-async program a)
        b (run-program-async program b)
        c (run-program-async program c)
        d (run-program-async program d)
        e (run-program-async program e)]
    (doseq [[from to] (partition 2 1 [a b c d e a])]
      (a/pipe (:out from) (:in to)))
    (a/put! (:in a) 0)
    (a/alts!! [(:state e) (a/timeout 5000)])
    (a/poll! (:in a))))

(defn find-max-thrust-with-feedback [program]
  (->> (combo/permutations [5 6 7 8 9])
       (map #(try-phase-settings-with-feedback program %))
       (apply max)))

(deftest amplification-circuit-with-feedback
  (are [program settings max]
       (= (try-phase-settings-with-feedback program settings) max)

    [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
     27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
    [9 8 7 6 5]
    139629729

    [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
     -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
     53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]
    [9 7 8 5 6]
    18216)

  (is (= (find-max-thrust-with-feedback
          (file->vec "2019-day-07-input.txt"))
         59597414)))

; --- Day 8: Space Image Format ---

(defn get-layers [file width height]
  (->> (get-input file)
       s/trim
       (map str)
       (map read-string)
       (partition (* width height))))

(defn get-min-layer [layers]
  (->> layers
       (map frequencies)
       (sort-by #(get % 0))
       first))

(defn apply-transparency [layers]
  (->> layers
       (reduce
        (fn [image layer]
          (merge
           (->> layer
                (map-indexed #(-> [%1 %2]))
                (filter #(not= (second %) 2))
                (into {}))
           image))
        {})
       (sort-by first)
       (map second)))

(defn layer->str [layer width]
  (->> layer
       (map #(if (= 0 %) " " "█"))
       (partition width)
       (map #(s/join "" %))
       (s/join "\n")))

(comment
  (-> (get-layers "2019-day-08-input.txt" 25 6)
      apply-transparency
      (layer->str 25)
      println))

(deftest space-image-format
  (is (=
       (let [layers (get-layers "2019-day-08-input.txt" 25 6)
             layer (get-min-layer layers)]
         (* (get layer 1) (get layer 2)))
       1452)))

; --- Day 9: Sensor Boost ---

(deftest sensor-boost
  (are [program in out]
       (= (run-program-new program in) out)
    [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99] [] 99
    [1102 34915192 34915192 7 4 7 99 0] [] 1219070632396864
    (file->vec "2019-day-09-input.txt") [1] 2775723069
    (file->vec "2019-day-09-input.txt") [2] 49115
    [104 1125899906842624 99] [] 1125899906842624))

; --- Day 10: Monitoring Station ---

(defn abs ^long [^long n] (max n (- n)))

(defn unit-vector [a b]
  (let [[x1 y1] a [x2 y2] b
        x (- x2 x1) y (- y2 y1)]
    (cond
      (= x y 0) [0 0]
      (= x 0)   [0 (if (neg? y) -1 1)]
      (= y 0)   [(if (neg? x) -1 1) 0]
      :else     [(/ x (abs y)) (/ y (abs x))])))

(deftest unit-vectors
  (are [a b unit]
       (= (unit-vector a b) unit)

    ; same point
    [0 0] [0 0] [0 0]

    ; diagonals
    [0 0] [1 1] [1 1]
    [0 0] [2 2] [1 1]
    [0 0] [3 3] [1 1]
    [1 1] [2 2] [1 1]

    [1 1] [0 0] [-1 -1]

    ; single direction
    [1 1] [1 2] [0 1]
    [1 1] [1 4] [0 1]
    [1 1] [1 8] [0 1]

    ; single direction
    [2 1] [1 1] [-1 0]
    [4 1] [1 1] [-1 0]
    [8 1] [1 1] [-1 0]

    ; other direction
    [0 0] [1 2] [1/2 2]
    [0 0] [2 4] [1/2 2]
    [1 1] [2 3] [1/2 2]
    [1 1] [3 5] [1/2 2]))

(defn parse-location-input [string]
  (->>
   (map-indexed
    (fn [row row-string]
      (map-indexed
       (fn [col c]
         (when (= c "#")
           [row col]))
       (s/split row-string #"")))
    (s/split string #"\n"))
   (apply concat)
   (filter identity)
   set))

(defn best-location [locations]
  (->>
   (map
    (fn [a]
      (group-by
       #(unit-vector a %)
       (disj locations a)))
    locations)
   (map count)
   (apply max)))

(deftest best-locations
  (is (= (->> (get-input "2019-day-10-input.txt")
              parse-location-input
              best-location)
         334)))

; --- Day 16: Flawed Frequency Transmission ---

(defn base-pattern [n]
  (->>
   (concat
    (repeat n 0) (repeat n 1) (repeat n 0) (repeat n -1))
   cycle
   rest))

(comment
  (time
   (let [input (get-input "2019-day-16-input.txt")
         input (parse-fft-input input)
         n 100
         out "42945143"]
     (c/quick-bench (FFT input n) :verbose)
     #_(= (take 8) (parse-fft-input out)))))

(defn aget-long ^long [^longs a ^long i] (aget a i))
(defn into-long-array [aseq] (into-array java.lang.Long/TYPE aseq))

(defn long-array-map [f a]
  (let [size (count a)
        result (long-array size)]
    (loop [i 0]
      (if (= i size)
        result
        (do
          (aset-long result i (f (get a i)))
          (recur (unchecked-add i 1)))))))

(defn FFT-1-row [pattern input]
  (let [size (count input)]
    (loop [n 0 acc 0]
      (if (= n size)
        (mod (Math/abs acc) 10)
        (let [i (aget-long input n)
              p (aget-long pattern n)]
          (recur
           (unchecked-add n 1)
           (unchecked-add acc (unchecked-multiply p i))))))))

(defn FFT-1 [patterns input]
  (long-array-map #(FFT-1-row % input) patterns))

(defn get-patterns [n]
  (->> (map inc (range))
       (map base-pattern)
       (take n)
       (map #(into-long-array (take n %)))
       (into [])))

(def get-patterns-memo (memoize get-patterns))

(defn FFT [input n]
  (let [patterns (get-patterns-memo (count input))
        input (into-long-array input)]
    (nth (iterate #(FFT-1 patterns %) input) n)))

(defn parse-fft-input [string]
  (mapv read-string (s/split (s/trim string) #"")))

(deftest flawed-frequency-transmission
  (are [input n out]
       (= (take 8 (FFT (parse-fft-input input) n))
          (parse-fft-input out))
    "12345678" 4 "01029498"
    "80871224585914546619083218645595" 100 "24176176"
    "19617804207202209144916044189917" 100 "73745418"
    "69317163492948606335995924319873" 100 "52432133"
    (get-input "2019-day-16-input.txt") 100 "42945143"))


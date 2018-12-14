(ns advent-of-code.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is run-tests]]))

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

; --- Day 3: No Matter How You Slice It ---

; https://adventofcode.com/2018/day/3

(defn read-claim [claim]
  (let [[id x y w h]
        (->> claim
             (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
             rest
             (map #(Integer/parseInt %)))]
    {:id id :x x :y y :w w :h h}))

(defn expand-claim [claim]
  (let [{:keys [x y w h]} claim]
    (assoc
     claim :coordinates
     (for [i (range w) j (range h)]
       {:x (+ x i) :y (+ y j)}))))

(defn count-intersect-claims [claims]
  (->> claims
       (mapcat :coordinates)
       frequencies
       (filter #(> (second %) 1))
       count))

; https://adventofcode.com/2018/day/3#part2

(defn uniq-claims [claims]
  (let [fabric (->> claims (mapcat :coordinates) frequencies)]
    (filter
     #(let [counts (->> % :coordinates (select-keys fabric) vals)]
        (= (count counts) (reduce + counts)))
     claims)))

(deftest no-matter-how-you-slice-it
  (let [d1 (->> ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"]
                (map read-claim)
                (map expand-claim))
        d2 (->> (s/split (slurp (io/resource "2018-day-03-input.txt")) #"\n")
                (map read-claim)
                (map expand-claim))]
    (doseq [[in out]
            [[d1 4] [d2 109143]]]
      (is (= (count-intersect-claims in) out)))
    (doseq [[in out]
            [[d1 3] [d2 506]]]
      (is (= (->> in uniq-claims first :id) out)))))

; --- Day 4: Repose Record ---

; https://adventofcode.com/2018/day/4

(defn read-event [event]
  (case event
    "wakes up" {:type :wake}
    "falls asleep" {:type :sleep}
    (let [[_ id] (re-find #"Guard #(\d+) begins shift" event)]
      {:type :begin :id (Integer/parseInt id)})))

(defn read-log [log]
  (->>
   (s/split log #"\n")
   (map
    (fn [entry]
      (let [found (rest (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)" entry))
            [year month day hour minute] (map #(Integer/parseInt %) (butlast found))
            event (last found)]
        (merge (read-event event)
               {:year year :month month
                :day day :hour hour :minute minute}))))
   (sort-by (juxt :year :month :day :hour :minute))))

(defn process-log-entry [state entry]
  (case (:type entry)
    (:begin :sleep)
    (merge state entry)
    :wake
    (let [times (range (:minute state) (:minute entry))
          asleep (into {} (map vector times (repeat 1)))]
      (update-in state
                 [:result (:id state)]
                 #(merge-with + % asleep)))))

(defn process-log [log]
  (:result (reduce process-log-entry {:result {}} log)))

(defn most-minutes-asleep [times]
  (first (sort-by #(->> % second vals (reduce +)) > times)))

(defn minute-most-likely-asleep [times]
  (first (sort-by second > times)))

(defn best-chance-of-sneaking-in [log]
  (let [times (process-log (read-log log))
        [id times] (most-minutes-asleep times)
        [minute] (minute-most-likely-asleep times)]
    (* id minute)))

(defn most-frequently-asleep [log]
  (let [times (process-log (read-log log))
        [id times]
        (first
         (sort-by #(apply max (vals (second %))) > times))
        [minute] (first (sort-by #(second %) > times))]
    (* id minute)))

(deftest repose-record
  (doseq [[in part1 part2]
          [[(s/join
             "\n"
             ["[1518-11-01 00:00] Guard #10 begins shift"
              "[1518-11-01 00:05] falls asleep"
              "[1518-11-01 00:25] wakes up"
              "[1518-11-01 00:30] falls asleep"
              "[1518-11-01 00:55] wakes up"
              "[1518-11-01 23:58] Guard #99 begins shift"
              "[1518-11-02 00:40] falls asleep"
              "[1518-11-02 00:50] wakes up"
              "[1518-11-03 00:05] Guard #10 begins shift"
              "[1518-11-03 00:24] falls asleep"
              "[1518-11-03 00:29] wakes up"
              "[1518-11-04 00:02] Guard #99 begins shift"
              "[1518-11-04 00:36] falls asleep"
              "[1518-11-04 00:46] wakes up"
              "[1518-11-05 00:03] Guard #99 begins shift"
              "[1518-11-05 00:45] falls asleep"
              "[1518-11-05 00:55] wakes up"]) 240 4455]
           [(slurp (io/resource "2018-day-04-input.txt")) 146622 31848]]]
    (is (= (best-chance-of-sneaking-in in) part1))
    (is (= (most-frequently-asleep in) part2))))

; --- Day 5: Alchemical Reduction ---

; https://adventofcode.com/2018/day/5

(defn toggle-case [s]
  (let [up (s/upper-case s)]
    (if-not (= up s) up (s/lower-case s))))

(defn reduce-polymer-1 [polymer]
  (let [n (count polymer)]
    (loop [i 1 output (transient [])]
      (if (> i n)
        (persistent! output)
        (let [a (get polymer (dec i))
              b (get polymer i)]
          (cond
            (= (toggle-case a) b) (recur (+ i 2) output)
            :else (recur (inc i) (conj! output a))))))))

(defn reduce-polymer [polymer]
  (->> polymer
       (iterate reduce-polymer-1)
       (reduce #(if (= %1 %2) (reduced %1) %2))))

(deftest alchemical-reduction
  (doseq [[in out]
          [["dabAcCaCBAcCcaDA" 10]
           [(s/trim (slurp (io/resource "2018-day-05-input.txt"))) 11264]]]
    (is (= (count (reduce-polymer (s/split in #""))) out))))

(defn -main [] (run-tests 'advent-of-code.core))

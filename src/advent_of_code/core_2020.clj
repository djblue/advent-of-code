(ns advent-of-code.core-2020
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

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

; --- Day 4: Passport Processing ---

(defn has-fields? [passport]
  (= (count (dissoc passport :cid)) 7))

(defn between? [number-string a b]
  (<= a (Integer/parseInt number-string) b))

(defn valid-height? [height]
  (when-let [[_ h unit] (re-matches #"(\d+)(cm|in)" height)]
    (let [h (Integer/parseInt h)]
      (if (= unit "cm")
        (<= 150 h 193)
        (<= 59 h 76)))))

(defn valid-passport? [{:keys [byr iyr eyr hgt hcl ecl pid] :as p}]
  (and (has-fields? p)
       (between? byr 1920 2002)
       (between? iyr 2010 2020)
       (between? eyr 2020 2030)
       (valid-height? hgt)
       (re-matches #"#[0-9a-fA-F]{6}" hcl)
       (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
       (re-matches #"\d{9}" pid)))

(defn parse-passports [string]
  (for [passport (str/split string #"\n\n")]
    (->> passport
         (re-seq #"([^: \n]+):([^: \n]+)")
         (map #(let [[_ k v] %] [(keyword k) v]))
         (into {}))))

(def passports
  (str/join
   "\n"
   ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    "byr:1937 iyr:2017 cid:147 hgt:183cm"
    ""
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
    "hcl:#cfa07d byr:1929"
    ""
    "hcl:#ae17e1 iyr:2013"
    "eyr:2024"
    "ecl:brn pid:760753108 byr:1931"
    "hgt:179cm"
    ""
    "hcl:#cfa07d eyr:2025 pid:166559648"
    "iyr:2011 ecl:brn hgt:59in"]))

(def day-04-input
  (-> "2020-day-04-input.txt" io/resource slurp))

(deftest valid-passports
  (is (= 2   (->> passports    parse-passports (filter has-fields?)     count)))
  (is (= 260 (->> day-04-input parse-passports (filter has-fields?)     count)))
  (is (= 153 (->> day-04-input parse-passports (filter valid-passport?) count))))

; --- Day 5: Binary Boarding ---

(defn boarding-pass-id [input]
  (Integer/parseInt
   (str/replace input #"." {"F" "0" "B" "1" "L" "0" "R" "1"}) 2))

(def day-05-input
  (-> "2020-day-05-input.txt" io/resource slurp))

(def all-boarding-pass-ids
  (->> day-05-input str/split-lines (map boarding-pass-id) (into #{})))

(defn get-missing-pass []
  (first
   (for [a "FB" b "FB" c "FB" d "FB"
         e "FB" f "FB" g "FB"
         h "LR" i "LR" j "LR"
         :let [input (str a b c d e f g h i j)
               id    (boarding-pass-id input)]
         :when (and (not (contains? all-boarding-pass-ids id))
                    (contains? all-boarding-pass-ids (dec id))
                    (contains? all-boarding-pass-ids (inc id)))]
     id)))

(deftest boarding-pass-id-tests
  (is (= 357 (boarding-pass-id "FBFBBFFRLR")))
  (is (= 567 (boarding-pass-id "BFFFBBFRRR")))
  (is (= 119 (boarding-pass-id "FFFBBBFRRR")))
  (is (= 820 (boarding-pass-id "BBFFBBFRLL")))
  (is (= 835 (apply max all-boarding-pass-ids)))
  (is (= 649 (get-missing-pass))))

; --- Day 6: Custom Customs ---

(def day-06-input
  (-> "2020-day-06-input.txt" io/resource slurp))

(defn parse-groups [input]
  (map str/split-lines (str/split input #"\n\n")))

(defn count-groups [f input]
  (reduce
   (fn [n group]
     (+ n (->> group (map set) (reduce f) count)))
   0
   (parse-groups input)))

(deftest custom-customs-tests
  (is (= 6534 (count-groups set/union        day-06-input)))
  (is (= 3402 (count-groups set/intersection day-06-input))))

; --- Day 7: Handy Haversacks ---

(defn parse-rules [rules-string]
  (into
   {}
   (for [item (str/split-lines rules-string)
         :let [[_ variant color contains]
               (re-matches #"([^ ]+) ([^ ]+) bags contain (.*)" item)
               contains (for [item (str/split contains #", ")
                              :let [[_ n variant color]
                                    (re-matches #"([^ ]+) ([^ ]+) ([^ ]+).*" item)]
                              :when (not= n "no")]
                          [(keyword variant color) (Integer/parseInt n)])]]
     [(keyword variant color) contains])))

(defn bags-with [rules bag]
  (count
   (filter
    (fn reachable? [targets]
      (some
       (fn [[target]]
         (or (= target bag)
             (reachable? (get rules target))))
       targets))
    (vals (dissoc rules bag)))))

(defn total-bags [rules bag]
  (reduce
   (fn [acc [target n]]
     (+ acc n (* n (total-bags rules target))))
   0
   (get rules bag)))

(def bag-rules
  (str
   "light red bags contain 1 bright white bag, 2 muted yellow bags.\n"
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n"
   "bright white bags contain 1 shiny gold bag.\n"
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n"
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n"
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n"
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n"
   "faded blue bags contain no other bags.\n"
   "dotted black bags contain no other bags.\n"))

(def day-07-input
  (-> "2020-day-07-input.txt" io/resource slurp))

(deftest handy-haversacks-tests
  (is (= 4      (bags-with  (parse-rules bag-rules)     :shiny/gold)))
  (is (= 278    (bags-with  (parse-rules day-07-input)  :shiny/gold)))
  (is (= 32     (total-bags (parse-rules bag-rules)     :shiny/gold)))
  (is (= 45157  (total-bags (parse-rules day-07-input)  :shiny/gold))))

; --- Day 8: Handheld Halting ---

(defn run-program [instructions]
  (loop [pc 0 a 0 ran? #{}]
    (let [p (get instructions pc) [inst op] p]
      (cond
        (= (count instructions) pc) [::term a]
        (ran? pc)                   [::loop a]
        :else
        (case inst
          :acc (recur (inc pc)  (+ a op) (conj ran? pc))
          :jmp (recur (+ pc op) a        (conj ran? pc))
          :nop (recur (inc pc)  a        (conj ran? pc)))))))

(defn parse-program [program-string]
  (vec
   (for [line (str/split-lines program-string)
         :let [[_ inst op] (re-matches #"([^ ]+) (.\d+)" line)]]
     [(keyword inst) (Integer/parseInt op)])))

(defn fix-program [program]
  (first
   (for [i (range (count program))
         :let [program (update-in
                        program [i 0]
                        #(if (= % :nop) :jmp :nop))
               [condition result] (run-program program)]
         :when (= condition ::term)]
     [condition result])))

(def day-08-input
  (-> "2020-day-08-input.txt" io/resource slurp))

(def sample-program
  (str
   "nop +0\n"
   "acc +1\n"
   "jmp +4\n"
   "acc +3\n"
   "jmp -3\n"
   "acc -99\n"
   "acc +1\n"
   "jmp -4\n"
   "acc +6\n"))

(deftest handheld-halting-tests
  (is (= [::loop 5]    (run-program (parse-program sample-program))))
  (is (= [::loop 1614] (run-program (parse-program day-08-input))))
  (is (= [::term 8]    (fix-program (parse-program sample-program))))
  (is (= [::term 1260] (fix-program (parse-program day-08-input)))))

; --- Day 9: Encoding Error ---

(def day-09-input
  (-> "2020-day-09-input.txt" io/resource slurp))

(defn find-invalid-number [input]
  (first
   (for [numbers (partition 26 1 input)
         :let [n       (last numbers)
               numbers (take 25 numbers)]
         :when (empty?
                (for [a numbers
                      b numbers
                      :when (= (+ a b) n)] true))]
     n)))

(defn encryption-weakness [input invalid-number]
  (first
   (for [n       (range 2 (count input))
         numbers (partition n 1 input)
         :when (= invalid-number (reduce + numbers))]
     (+ (apply min numbers)
        (apply max numbers)))))

(deftest encoding-error-tests
  (let [input (read-string (str "[" day-09-input "]"))]
    (is (= 75678618
           (encryption-weakness
            input
            (find-invalid-number input))))))

; --- Day 10: Adapter Array ---

(def day-10-input
  (sort (read-string (str "[" (-> "2020-day-10-input.txt" io/resource slurp) "]"))))

(defn number-of-jolts [input]
  (->> (partition 2 1 input)
       (group-by (fn [[b a]] (- a b)))
       vals
       (map (comp inc count))
       (reduce *)))

(defn distinct-adapters [input]
  (->> (partition 2 1 (into [0] input))
       (map (fn [[b a]] (- a b)))
       (partition-by #{3})
       (remove (comp #{3} first))
       (map (comp [1 1 2 4 7] count))
       (reduce *)))

(deftest adapter-array-tests
  (is (= (number-of-jolts   day-10-input) 2812))
  (is (= (distinct-adapters day-10-input) 386869246296064)))

; --- Day 11: Adapter Array ---

(defn parse-seats [string]
  (let [lines  (str/split-lines string)
        height (count lines)
        width  (count (first lines))]
    (into
     {}
     (for [row    (range height)
           column (range width)]
       [{:row row :column column}
        (case (get-in lines [row column])
          \L :empty
          \. :floor)]))))

(defn occupied-1 [plane {:keys [row column]}]
  (reduce
   (fn [n position]
     (if (= :occupied (get plane position)) (inc n) n))
   0
   [{:row (inc row) :column column}
    {:row (dec row) :column column}
    {:row row :column (inc column)}
    {:row row :column (dec column)}
    {:row (inc row) :column (inc column)}
    {:row (dec row) :column (dec column)}
    {:row (inc row) :column (dec column)}
    {:row (dec row) :column (inc column)}]))

(defn adjacent-search [plane {:keys [row column]} update-row update-column]
  (let [position {:row    (update-row row)
                  :column (update-column column)}
        value    (get plane position)]
    (cond
      (= value :occupied) 1
      (= value :floor)
      (recur plane position update-row update-column)
      :else 0)))

(defn occupied-2 [plane position]
  (+
   (adjacent-search plane position inc identity)
   (adjacent-search plane position dec identity)
   (adjacent-search plane position identity inc)
   (adjacent-search plane position identity dec)
   (adjacent-search plane position inc inc)
   (adjacent-search plane position dec dec)
   (adjacent-search plane position inc dec)
   (adjacent-search plane position dec inc)))

(defn next-plane [f n plane]
  (persistent!
   (reduce-kv
    (fn [next-plane position state]
      (let [occupied (f plane position)]
        (cond
          (and (= state :empty) (zero? occupied))
          (assoc! next-plane position :occupied)

          (and (= state :occupied) (>= occupied n))
          (assoc! next-plane position :empty)

          :else next-plane)))
    (transient plane)
    plane)))

(defn stable-steats [f n plane]
  (->> (iterate
        (partial next-plane f n) plane)
       (take 200)
       (partition 2 1)
       (filter (fn [[a b]] (= a b)))
       ffirst
       vals
       frequencies
       :occupied))

(def day-11-input
  (-> "2020-day-11-input.txt" io/resource slurp))

(def example-seats
  (str
   "L.LL.LL.LL\n"
   "LLLLLLL.LL\n"
   "L.L.L..L..\n"
   "LLLL.LL.LL\n"
   "L.LL.LL.LL\n"
   "L.LLLLL.LL\n"
   "..L.L.....\n"
   "LLLLLLLLLL\n"
   "L.LLLLLL.L\n"
   "L.LLLLL.LL\n"))

(deftest seating-system-tests
  (is (= 37   (stable-steats occupied-1 4 (parse-seats example-seats))))
  (is (= 2418 (stable-steats occupied-1 4 (parse-seats day-11-input))))
  (is (= 26   (stable-steats occupied-2 5 (parse-seats example-seats))))
  (is (= 2144 (stable-steats occupied-2 5 (parse-seats day-11-input)))))

; --- Day 12: Rain Risk ---

(def day-12-input
  (-> "2020-day-12-input.txt" io/resource slurp))

(def inst->keyword
  {"N" :north
   "S" :south
   "E" :east
   "W" :west
   "L" :left
   "R" :right
   "F" :forward})

(defn parse-actions [string]
  (for [line (str/split-lines string)
        :let [inst (subs line 0 1)
              n    (subs line 1)]]
    [(inst->keyword inst) (Integer/parseInt n)]))

(defn add [a b] (mapv + a b))
(defn sub [a b] (mapv - a b))
(defn mul [a n] (mapv #(* n %) a))

(defn rotate [[x y] degrees]
  (case (mod degrees 360)
    0   [x y]
    90  [y (- x)]
    180 [(- x) (- y)]
    270 [(- y) x]))

(defn move [position [inst n]]
  (case inst
    :north (add position [(+ n) 0])
    :south (add position [(- n) 0])
    :east  (add position [0 (+ n)])
    :west  (add position [0 (- n)])))

(def direction->inst
  {0 :east 90 :north 180 :west 270 :south})

(defn apply-ship-instructions [instructions]
  (:position
   (reduce
    (fn [state [inst n]]
      (case inst
        (:north :south :east  :west)
        (update state :position move [inst n])

        :left   (update state :direction #(mod (+ % n) 360))
        :right  (update state :direction #(mod (- % n) 360))

        :forward
        (update state :position move [(direction->inst (:direction state)) n])))
    {:direction 0
     :position [0 0]}
    instructions)))

(defn apply-waypoint-instructions [instructions]
  (:position
   (reduce
    (fn [state [inst n]]
      (case inst
        (:north :south :east  :west)
        (update state :waypoint move [inst n])

        :left   (update state :waypoint rotate n)
        :right  (update state :waypoint rotate (- n))

        :forward
        (update state :position add (mul (:waypoint state) n))))
    {:direction 0
     :waypoint [1 10]
     :position [0 0]}
    instructions)))

(defn manhattan-distance [a]
  (reduce #(+ %1 (Math/abs %2)) 0 a))

(deftest rain-risk-tests
  (is
   (=
    286
    (manhattan-distance
     (apply-waypoint-instructions
      [[:forward 10] [:north 3] [:forward 7] [:right 90] [:forward 11]]))))
  (is (= 1133  (-> day-12-input parse-actions apply-ship-instructions     manhattan-distance)))
  (is (= 61053 (-> day-12-input parse-actions apply-waypoint-instructions manhattan-distance))))

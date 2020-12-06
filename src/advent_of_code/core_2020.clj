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

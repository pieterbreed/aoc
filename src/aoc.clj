(ns aoc
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clj-http.client :as http]
            [clojure.set :as set]
            [clojure.test :as t :refer [deftest is]]
            [odoyle.rules :as o]))

(defonce session-cookie (atom (System/getenv "AOC_COOKIE")))

(defn day-input-2022 [day]
  (:body (http/get (format "https://adventofcode.com/2022/day/%d/input" day)
                   {:cookies {"session" {:value @session-cookie}}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day 1
;; Find the elf carrying the most calories
;; https://adventofcode.com/2022/day/1

(defn day1-1
  "Input is a string.
  Items are seperated by newlines. Items represent calories.
  Groups are seperated by a blank line. Groups represent total calories carried by an Elf.
  The question is which Elf carries the most calories."
  [input]
  (->> input
       (str/split-lines)

       ;; group the sequence by noticing empty lines
       (reduce (fn [[result current] item]
                 (if (= "" item)
                   [(conj result current) []]
                   [result (conj current item)]))
               [[] []])

       ;; append the list item to the accumulator because no empty line at end of input
       ;; return only the accumulator
       ((fn [[result last-bunch]] (conj result last-bunch)))

       ;; sum every group
       (map (fn [items-in-group]
              (->> items-in-group
                   (map #(Integer/parseInt %))
                   (reduce +))))

       ;; sort highest to lowest
       (sort >)))

(comment
  (def test-input
        "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")
  (day1-1 "1000
2000

3000")
  (day1 test-input)

  ;; first solution
  (-> (slurp (io/resource "day1.txt"))
      (day1-1)
      first) ;; 72602

  ;; second solution
  (->> (slurp (io/resource "day1.txt"))
       (day1-1)
       (take 3)
       (apply +));; 207410

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day 2
;; calculate the score for rock, paper, scissors strategy guide
;; A - Rock, B - Paper, C - Scissors
;; X - Rock, Y - Paper, Z - Scissors

(def symbols-take-1
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(defn day2-read-strategy-guide-take-1
  [strategy-line]
  (->> (str/split strategy-line #" ")
       (mapv symbols-take-1)))

(def day2-round-result
  {[:rock :rock]         3
   [:rock :paper]        6
   [:rock :scissors]     0
   [:paper :rock]        0
   [:paper :paper]       3
   [:paper :scissors]    6
   [:scissors :rock]     6
   [:scissors :paper]    0
   [:scissors :scissors] 3})

(defn day2-round-score [round]
  (let [[_ play] round]
    ({:rock     1
      :paper    2
      :scissors 3} play)))

(defn day2-score-strategy [strategy]
  (->> (str/split-lines strategy)
       (filter seq)
       (map day2-read-strategy-guide-take-1)
       (map (juxt day2-round-result day2-round-score))
       (map #(reduce + %))
       (reduce +)
       ))

(comment

  (def day2-input (day-input-2022 2))
  (day2-score-strategy day2-input) ;; 13924

  )

(def symbols-take-2
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :lose
   "Y" :draw
   "Z" :win})

(defn day2-read-strategy-guide-take-2
  [strategy-line]
  (->> (str/split strategy-line #" ")
       (mapv symbols-take-2)))

(def day2-reverse-play
  {[:rock :lose]     :scissors
   [:rock :draw]     :rock
   [:rock :win]      :paper
   [:paper :lose]    :rock
   [:paper :draw]    :paper
   [:paper :win]     :scissors
   [:scissors :lose] :paper
   [:scissors :draw] :scissors
   [:scissors :win]  :rock})

(defn day2-score-strategy-take-2 [strategy]
  (->> (str/split-lines strategy)
       (filter seq)
       (map day2-read-strategy-guide-take-2)
       (map (fn [[opp res :as play]] [opp (day2-reverse-play play)]))
       (map (juxt day2-round-result day2-round-score))
       (map #(reduce + %))
       (reduce +)
       ))

(comment

  (day2-score-strategy-take-2 day2-input) ;; 13448
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day 3-1
;; find the item in both compartments of a rucksack

(defn d3-item-type-values
  "Takes an item type and looks up its value.

  a-z => 1-26
  A-Z => 26-52"
  [it]
  (-> "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      (str/index-of it)
      inc))

(defn d3-find-common-item
  "Find the common item (a letter) that exists in both halves of the input (a string)"
  [items]
  (let [nr-items (count items)
        half-index (/ nr-items 2)
        s1 (set (subs items 0 half-index))
        s2 (set (subs items half-index nr-items))
        common-items (set/intersection s1 s2)]
    (assert (= 1 (count common-items))
            (str "There must only be one item in common according to the specs:\n"
                 common-items))
    (-> common-items first str)))

(comment

  (d3-find-common-item "vJrwpWtwJgWrhcsFMMfFFhFp")
  (d3-find-common-item "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")

  (d3-find-common-item "PmmdzqPrVvPwwTWBwg")
  (d3-find-common-item "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
  (d3-find-common-item "ttgJtRGJQctTZtZT")
  (d3-find-common-item "CrZsJsPPZsGzwwsLwLmpwMDw")

  )

(defn d3-sum-of-priorities
  "Takes a list of rucksack contents (new-line seperated lines).
  For each rucksack contents, find the item in common in both halves,
  find the priority, sum all teh priorities together"
  [rucksacks]
  (->> rucksacks
       (str/split-lines)
       (map d3-find-common-item)
       (map d3-item-type-values)
       (reduce +)))

(comment

  (def test-rucksacks "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")
  (d3-sum-of-priorities test-rucksacks)

  (def day-3-input (day-input-2022 3))
  (d3-sum-of-priorities day-3-input) ;; 7863
  )

(defn d3-sum-of-priorities-for-badges
  "Lines come in groups of 3, one item type is common to all three rucksacks, find it, find its priority, sum those up."
  [rucksacks]
  (->> rucksacks
       (str/split-lines)
       (partition 3)
       (map (fn [[a b c]]
              (str (first (set/intersection (set a) (set b) (set c))))))
       (map d3-item-type-values)
       (reduce +)))

(comment

  (d3-sum-of-priorities-for-badges test-rucksacks)
  (d3-sum-of-priorities-for-badges day-3-input) ;; 2488

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 4
;;

(def d4-test-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-range
  "Takes x-y and return [x y]"
  [r]
  (let [[a b] (str/split r #"-")]
    [(Integer/parseInt a)
     (Integer/parseInt b)]))

(defn d4-prepare-input
  "Split lines, make 2 groups, represent boundaries of sections"
  [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #","))
       (map (juxt (comp parse-range first)
                  (comp parse-range second)))))

(defn ranges-fully-overlap?
  "Takes two ranges, already parsed out, returns truthy if one is completely inside another"
  [[[af at] [bf bt]]]
  (or (<= af bf bt at)
      (<= bf af at bt)))

(defn ranges-partially-overlap?
  "Takes two ranges, already parsed out, returns truthy if one is completely inside another"
  [[[af at] [bf bt]]]
  (or (<= af bf at) (<= af bt at)
      (<= bf af bt) (<= bf at bt)))

(defn d4-find-fully-overlapping-ranges
  [input p]
  (->> (d4-prepare-input input)
       (filter p)))

(comment

  (def d4-input (day-input-2022 4))
  (count (d4-find-fully-overlapping-ranges d4-input
                                           ranges-fully-overlap?))

  (count (d4-find-fully-overlapping-ranges d4-input
                                           ranges-partially-overlap?)) ;; 845
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 5 - supply stacks

(def d5-test-input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(comment

  (def d5-input (day-input-2022 5))

  )

;; simplifying assumption - less than 10 stacks
(defn- d5-create-column-stack-indexes
  [sketch-lines]
  (->> (last sketch-lines)
       (map vector (range))
       (filter (comp #(not= % \space) second))
       (map reverse)
       (map (fn [[c i]] [(Integer/parseInt (str c)) i]))
       (into {})))

;; flips the data structure on its side
;; the stacks or the indices to the result vector
;; the crates are then in heigh order
(defn d5-parse-initial-arrangement
  [sketch-lines]
  (vec (let [column-stack-indexes (d5-create-column-stack-indexes sketch-lines)]
         (->> (for [l (->> (butlast sketch-lines)
                           (reverse)
                           (map-indexed vector))
                    i (keys column-stack-indexes)
                    :let [crate (-> l second (get (column-stack-indexes i)))]
                    :when (and crate
                               (not= \space crate))]
                {:height (first l)
                 :stack  i
                 :crate  crate})
              (group-by :stack)
              (sort-by first)
              (map (fn [[stack proto-crates]]
                     (vec (->> proto-crates
                               (sort-by :height)
                               (map :crate)))))))))

(comment

  (def d5-test-arrangement (take-while (comp pos? count)
                                       (str/split-lines d5-test-input)))
  (def d5-test-instructions (rest (drop-while (comp pos? count)
                                              (str/split-lines d5-test-input))))
  (d5-parse-initial-arrangement d5-test-arrangement)
  )

(defn d5-parse-instructions
  [instructions-lines]
  (->> instructions-lines
       (map #(str/split % #" "))
       (map #(->> (partition 2 %)
                  (map second)))
       (map (fn [[move from to]]
              [(Integer/parseInt move)
               (dec (Integer/parseInt from))
               (dec (Integer/parseInt to))]))))

(comment

  (d5-parse-instructions d5-test-instructions)

  )



(defn d5-1-instruction [state [move from to]]
  (if (= 0 move) state
      (-> state
          (update from #(vec (butlast %)))
          (update to #(vec (concat % [(-> state (get from) last)])))
          (d5-1-instruction [(dec move)
                           from
                           to]))))

(comment

  (-> (d5-parse-initial-arrangement d5-test-arrangement)
      (d5-instruction (first (d5-parse-instructions d5-test-instructions))))

  )

(defn d5-move-stacks
  "two steps
  - build the initial data structure; which crates are on which stacks
  - dsl for moving crates around"
  [input how]
  (let [input-lines (str/split-lines input)
        initial-arrangement (d5-parse-initial-arrangement (take-while (comp pos? count)
                                                                       input-lines))
        instructions (d5-parse-instructions (rest (drop-while (comp pos? count)
                                                              input-lines)))]
    (reduce how initial-arrangement instructions)))

(defn d5-1-solution [instructions]
  (let [result-state (d5-move-stacks instructions d5-1-instruction)]
    (apply str (->> result-state
                    (map last)))))

(defn d5-2-instruction [state [move from to]]
  (let [crates (->> (get state from)
                    (reverse)
                    (take move)
                    (reverse))]
    (-> state
        (update from #(vec (->> %
                                (reverse)
                                (drop move)
                                (reverse))))
        (update to #(vec (concat % crates))))))

(defn d5-2-solution [instructions]
  (let [result-state (d5-move-stacks instructions d5-2-instruction)]
    (apply str (->> result-state
                    (map last)))))

(comment


  (d5-1-solution d5-test-input) ;; "CMZ"
  (d5-2-solution d5-test-input) ;; "MCD"

  (def d5-input (day-input-2022 5))

  (d5-1-solution d5-input) ;; "QNHWJVJZW"
  (d5-2-solution d5-input) ;; "BPCZJLFJW"



  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 6
;; unique sequence of 4 characters in a stream

(def d6-test-input-1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def d6-test-input-2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def d6-test-input-3 "nppdvjthqldpwncqszvftbrmjlhg")
(def d6-test-input-4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def d6-test-input-5 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(defn d6 [input packet-size]
  (let [[i _] (->> (map vector
                        (range)
                        (partition packet-size 1 input))
                   (filter (fn [[_ chars]] (= packet-size (count (set chars)))))
                   first)]
    (+ i packet-size)))

(def d6-1 #(d6 % 4))
(def d6-2 #(d6 % 14))

(comment

  (def d6-input (day-input-2022 6))

  (d6-1 d6-test-input-1);; 7
  (d6-1 d6-test-input-2);; 5
  (d6-1 d6-test-input-3);; 6
  (d6-1 d6-test-input-4);; 10
  (d6-1 d6-test-input-5);; 11

  (d6-1 d6-input) ;; 1929

  (d6-2 d6-input);; 3298

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 7
;;

(def d7-test-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def d7-default-state
  {:current-dir []
   :fs          {}})

(defn d7-parse-line
  "Takes a state item and input with output, and updates the state"
  [s [cmd & output]]
  (let [cmd' (subs cmd 2)]
    (cond
      (str/starts-with? cmd' "cd")
      (let [dest (-> cmd'
                     (str/split #" ")
                     second)]
        (cond
          (= "/" dest)
          (assoc s :current-dir [])

          (= ".." dest)
          (update s :current-dir (comp vec butlast))

          :else
          (update s :current-dir conj dest)))

      (= "ls" cmd')
      (update-in s (concat [:fs] (:current-dir s))
                 merge
                 (->> output
                      (map (fn [ls-output-line]
                             (let [[dir-or-size name] (str/split ls-output-line #" ")]
                               (if-not (= "dir" dir-or-size)
                                 [name (Integer/parseInt dir-or-size)]))))
                      (into {}))))))

(comment

  (d7-parse-line d7-default-state
                 ["$ cd /"])

  (d7-parse-line d7-default-state
                 ["$ cd a"])

  (d7-parse-line d7-default-state
                 ["$ ls" "dir e" "29116 f" "2557 g" "62596 h.lst"])
  {:current-dir []
   :fs          {"f"     29116
                 "g"     2557
                 "h.lst" 62596}}

  )

(defn d7-split-input
  "Takes input; split it in groups by input and associating output with correct input in a group"
  [input]
  (let [{:keys [current
                history]}
        (->> input
             (str/split-lines)
             (reduce (fn [{:keys [history
                                  current]
                           :as   acc}
                          l]
                       (if (str/starts-with? l "$")
                         (cond-> acc
                           current (update :history conj current)
                           true    (assoc  :current [l]))
                         (update acc :current conj l)))
                     {:history []
                      :current nil}))]
    (cond-> history
      current (conj current))))

(defn d7-build-fs [input]
  (->> input
       d7-split-input
       (reduce d7-parse-line
               d7-default-state)))

(defn d7-recurse-fs
  "Takes an fs (map) and summarizes each directory"
  ([fs summarize-fn] (d7-recurse-fs fs summarize-fn nil))
  ([fs summarize-fn path]
   (let [{subdirs true
          files   false}
         (group-by (comp map? second) fs)

         subdir-summaries (->> subdirs
                               (mapcat (fn [[n sd]]
                                         (d7-recurse-fs sd
                                                        summarize-fn
                                                        (conj (vec path) n)))))

         files-summary [path (summarize-fn (concat files subdir-summaries))]]
     (vec (concat
           [files-summary]
           subdir-summaries)))))

(deftest d7-recurse-fs-test-1
  (is (= [[nil 123]]
         (d7-recurse-fs {"a" 100
                         "b" 23}
                        (fn [xs]
                          (->> xs
                               (filter (comp string? first))
                               (map second)
                               (reduce + 0)))))))

(deftest d7-recurse-fs-test2
  (is (= [[nil   123]
          [["c"] 15]
          [["f"] 20]]

         (d7-recurse-fs {"a" 100
                         "b" 23
                         "c" {"d" 10
                              "e" 5}
                         "f" {"g" 20}}
                        (fn [xs]
                          (->> xs
                               (filter (comp string? first))
                               (map second)
                               (reduce + 0)))))))

(deftest d7-recurse-fs-test3
  (is (= [[nil   123]
          [["c"] 15]
          [["c" "f"] 20]]

         (d7-recurse-fs {"a" 100
                         "b" 23
                         "c" {"d" 10
                              "e" 5
                              "f" {"g" 20}}}
                        (fn [xs]
                          (->> xs
                               (filter (comp string? first))
                               (map second)
                               (reduce + 0)))))))

(deftest d7-recurse-fs-include-subdirs-in-summary-test
  (is (= [[nil   (+ 100 23
                    (+ 10 5 20)
                    20)]
          [["c"] (+ 10 5 20)]
          [["c" "f"] 20]]

         (d7-recurse-fs {"a" 100
                         "b" 23
                         "c" {"d" 10
                              "e" 5
                              "f" {"g" 20}}}
                        (fn [xs]
                          (->> xs
                               ;; (filter (comp string? first))
                               (map second)
                               (reduce + 0)))))))

(defn d7-solve-1
  [input]
  (let [fs (:fs (d7-build-fs input))
        summaries (d7-recurse-fs fs
                                 (fn [xs]
                                   (->> xs
                                        (map second)
                                        (reduce + 0))))]
    (->> summaries
         (filter (comp #(< % 100000) second))
         (map second)
         (reduce + 0))))

(defn d7-total-fs-usage
  [fs]
  (let [summary-fn (fn [xs]
                     (->> xs
                          (filter (comp string? first))
                          (map second)
                          (reduce + 0)))
        summary (d7-recurse-fs fs
                               summary-fn)]
    (->> summary
         (map second)
         (reduce + 0))))

(defn d7-solve-2 [input]
  (let [total-disk-space 70000000
        required-disk-space 30000000

        fs (:fs (d7-build-fs input))
        total-used (d7-total-fs-usage fs)
        current-free (- total-disk-space total-used)
        require-at-least (- required-disk-space current-free)

        summary-fn (fn [xs]
                     (->> xs
                          (filter (comp string? first))
                          (map second)
                          (reduce + 0)))
        summary (d7-recurse-fs fs
                               summary-fn)]
    (->> summary
         (map (fn [[path _]] [path (d7-total-fs-usage (get-in fs path))]))
         (filter (fn [[_ consumed-space]] (< require-at-least consumed-space)))
         (sort-by second)
         first
         second)))

(comment

  (d7-total-fs-usage (:fs (d7-build-fs d7-test-input))) ;; 48381165


  (d7-recurse-fs (:fs (d7-build-fs d7-test-input))
                 (fn [xs]
                   (->> xs
                        (map second)
                        (filter #(< % 100000))
                        (reduce + 0))))

  (d7-solve-1 d7-test-input) ;; 95437
  (def d7-input (day-input-2022 7))
  (d7-solve-1 d7-input) ;; 1118405
  (d7-solve-2 d7-test-input) ;; 24933642
  (d7-solve-2 d7-input) ;; 12545514

  (d7-split-input d7-test-input)
  [["$ cd /"]
   ["$ ls" "dir a" "14848514 b.txt" "8504156 c.dat" "dir d"]
   ["$ cd a"]
   ["$ ls" "dir e" "29116 f" "2557 g" "62596 h.lst"]
   ["$ cd e"]
   ["$ ls" "584 i"]
   ["$ cd .."]
   ["$ cd .."]
   ["$ cd d"]
   ["$ ls" "4060174 j" "8033020 d.log" "5626152 d.ext" "7214296 k"]
   ]

  (d7-build-fs d7-test-input)
  {:current-dir ["d"]
   :fs          {"b.txt" 14848514
                 "c.dat" 8504156
                 "a"     {"f"     29116
                          "g"     2557
                          "h.lst" 62596
                          "e"     {"i" 584}}
                 "d"     {"j"     4060174
                          "d.log" 8033020
                          "d.ext" 5626152
                          "k"     7214296}
                 }}

  (d7-solve-1 d7-test-input)

  (->> (map-zipper (:fs (d7-build-fs d7-test-input)))
       (iterate zip/next)
       (take-while (complement zip/end?))
       (filter zip/branch?)
       ;; count
       (map (juxt zip/path zip/node))
       )

  (-> (map-zipper (:fs (d7-build-fs d7-test-input)))
      (zip/down)
      (zip/branch?))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 8
;; trees in a grid

(def d8-tree-heights
  {\0 0
   \1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9})

(defn d8-parse-input
  [input]
  (->> input
       (str/split-lines)
       (map #(map d8-tree-heights %))))

(defn d8-dimensions
  "takes a parsed tree-map (seq of seq of ints) and determines the dimension in height and width. assumes all widths are the same"
  [tree-map]
  {:width  (-> tree-map first count)
   :height (-> tree-map count)})

(defn d8-heights-fns [tree-map x y]
  (let [{:keys [height
                width]}
        (d8-dimensions tree-map)]
    {:heights/north #(->> (range y -1 -1)  (map (fn [y'] (-> tree-map (nth y') (nth x)))))
     :heights/south #(->> (range y height) (map (fn [y'] (-> tree-map (nth y') (nth x)))))
     :heights/west  #(->> (range x -1 -1)  (map (fn [x'] (-> tree-map (nth y)  (nth x')))))
     :heights/east  #(->> (range x width)  (map (fn [x'] (-> tree-map (nth y)  (nth x')))))}))

(defn first-is-visible-from-outside
  [[h & rst]]
  (->> rst
       (filter (fn [x] (>= x h)))
       (count)
       (zero?)))

(defn d8-is-visible?
  "Low-effort, probably n^3 behaviour. Brute-force algorithm."
  [tree-map x y]
  (let [{:keys [width height]}
        (d8-dimensions tree-map)

        {:heights/keys [north
                        south
                        west
                        east]}
        (d8-heights-fns tree-map x y)]
    (or (= 0 x)
        (= 0 y)
        (= (dec width) x)
        (= (dec height) y)
        (first-is-visible-from-outside (north))
        (first-is-visible-from-outside (south))
        (first-is-visible-from-outside (east))
        (first-is-visible-from-outside (west)))))

(def d8-test-input
  "30373
25512
65332
33549
35390")
(def d8-test-treemap (d8-parse-input d8-test-input))
(deftest d8-1
  (is (not (d8-is-visible? d8-test-treemap 2 2)))
  (is (not (d8-is-visible? d8-test-treemap 3 1)))
  (is (not (d8-is-visible? d8-test-treemap 1 3)))
  (is (not (d8-is-visible? d8-test-treemap 3 3)))
  (is (d8-is-visible? d8-test-treemap 1 1))
  (is (d8-is-visible? d8-test-treemap 2 1))
  (is (d8-is-visible? d8-test-treemap 1 2))
  (is (d8-is-visible? d8-test-treemap 3 2)))

(defn d8-solve-1
  "Count visible trees"
  [input]
  (let [tree-map (d8-parse-input input)
        {:keys [height
                width]}  (d8-dimensions tree-map)]
    (count (for [x (range height)
                 y (range width)
                 :when (d8-is-visible? tree-map x y)]
             :visible)))
  )

(comment

  (d8-solve-1 d8-test-input) ;;
  (def d8-input (day-input-2022 8))
  (time (d8-solve-1 d8-input)) ;; 1818
  ;; "Elapsed time: 1917.873636 msecs"
  ;; "Elapsed time: 1880.310128 msecs"
  ;; "Elapsed time: 1879.676464 msecs"
  ;; "Elapsed time: 1867.073619 msecs"

  (d8-is-visible? d8-test-treemap 2 4)
  (d8-is-visible? d8-test-treemap 2 2)

  (def d8-test-input'
    "30373
25512
65332
65332
33549
35390")

  (-> (d8-parse-input d8-test-input')
      d8-dimensions)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 8-2

(defn take-until
  ([p coll]
   (reduce (fn [acc x]
             (let [next (concat acc [x])]
               (if (p x)
                 (reduced next)
                 next)))
           '()
           coll)))

(deftest take-until-tests
  (is (= (seq [1 2 3 4 6])
         (seq (take-until #(>= % 5) [1 2 3 4 6 4 3]))))
  (is (= (seq [1 2 3 4 4 3])
         (seq (take-until #(>= % 5) [1 2 3 4  4 3]))))
  (is (= (seq [1 2 3 4 5])
         (seq (take-until #(>= % 5) [1 2 3 4 5])))))

(defn d8-part-2-distance-can-see-in-direction
  [direction]
  (let [[this-h & rst] (direction)]
    (->> rst
         (take-until (fn [h] (>= h this-h)))
         (count))))

(defn d8-viewing-distances [tree-map x y]
  (let [{:heights/keys [north
                        south
                        west
                        east]}
        (d8-heights-fns tree-map x y)]

    (* (d8-part-2-distance-can-see-in-direction north)
       (d8-part-2-distance-can-see-in-direction south)
       (d8-part-2-distance-can-see-in-direction west)
       (d8-part-2-distance-can-see-in-direction east))))


(deftest d8-part-2-viewing-distances-test
  (is (= 4 (d8-viewing-distances (d8-parse-input d8-test-input)
                                 2 1)))
  (is (= 8 (d8-viewing-distances (d8-parse-input d8-test-input)
                                 2 3))))


(defn d8-part-2-solve
  [input]
  (let [tree-map
        (d8-parse-input input)

        {:keys [height width]}
        (d8-dimensions tree-map)]
    (->> (for [x (range 1 (dec width))
               y (range 1 (dec height))]
           (d8-viewing-distances tree-map x y))
         (sort)
         (reverse)
         (first))))

(comment

  (d8-part-2-solve d8-test-input)
  (d8-part-2-solve d8-input) ;; 368368

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 9
;; rope bridge

(defn d9-h-and-t-valid?
  [[hx hy] [tx ty]]
  (and (<= (dec hx) tx (inc hx))
       (<= (dec hy) ty (inc hy))))

(deftest d9-adjacent-tests
  (is (d9-h-and-t-valid? [0 0] [0 0]))
  (is (d9-h-and-t-valid? [1 0] [0 0]))
  (is (d9-h-and-t-valid? [-1 0] [0 0]))
  (is (d9-h-and-t-valid? [0 0] [0 1]))
  (is (d9-h-and-t-valid? [0 0] [0 -1]))
  (is (d9-h-and-t-valid? [5 5] [5 4]))
  (is (d9-h-and-t-valid? [5 5] [4 5]))

  (is (not (d9-h-and-t-valid? [-1 -2] [0 -1]))))

(defn d9-north [[x y]] [x (- y 2)])
(defn d9-south [[x y]] [x (+ y 2)])
(defn d9-east  [[x y]] [(+ x 2) y])
(defn d9-west  [[x y]] [(- x 2) y])

(defn d9-north-of? [a b] (= a (d9-north b)))
(defn d9-south-of? [a b] (= a (d9-south b)))
(defn d9-west-of?  [a b] (= a (d9-west  b)))
(defn d9-east-of?  [a b] (= a (d9-east  b)))

(defn d9-northeast-of?
  [[ax ay]
   [bx by]]
  (or (= [ax       ay]
         [(+ bx 1) (- by 2)])
      (= [ax       ay]
         [(+ bx 2) (- by 1)])))

(defn d9-northwest-of?
  [[ax ay]
   [bx by]]
  (or (= [ax       ay]
         [(- bx 1) (- by 2)])
      (= [ax       ay]
         [(- bx 2) (- by 1)])))

(defn d9-southeast-of?
  [[ax ay]
   [bx by]]
  (or (= [ax       ay]
         [(+ bx 1) (+ by 2)])
      (= [ax       ay]
         [(+ bx 2) (+ by 1)])))

(defn d9-southwest-of?
  [[ax ay]
   [bx by]]
  (or (= [ax       ay]
         [(- bx 1) (+ by 2)])
      (= [ax       ay]
         [(- bx 2) (+ by 1)])))

(deftest d9-directions-test
  (is (d9-north-of? [0 -2] [0 0]))
  (is (d9-south-of? [0  2] [0 0]))
  (is (d9-west-of?  [-2 0] [0 0]))
  (is (d9-east-of?  [2  0] [0 0]))

  (is (d9-northeast-of? [1 -2] [0 0]))
  (is (d9-northeast-of? [2 -1] [0 0]))
  (is (d9-northeast-of? [2 -1] [0 0]))

  (is (d9-northeast-of? [1 -2] [0 -1]))

  (is (not (d9-northwest-of? [1 -2] [0 0])))
  (is (not (d9-northwest-of? [2 -1] [0 0])))
  (is (d9-northwest-of? [-1 -2] [0 0]))
  (is (d9-northwest-of? [-2 -1] [0 0]))

  (is (not (d9-southeast-of? [1 -2] [0 0])))
  (is (not (d9-southeast-of? [2 -1] [0 0])))
  (is (d9-southeast-of? [1 2] [0 0]))
  (is (d9-southeast-of? [2 1] [0 0]))

  (is (not (d9-southwest-of? [1 -2] [0 0])))
  (is (not (d9-southwest-of? [2 -1] [0 0])))
  (is (d9-southwest-of? [-1 2] [0 0]))
  (is (d9-southwest-of? [-2 1] [0 0])))

(def d9-rules
  (o/ruleset
   {::head
    [:what
     [::head ::x head-x]
     [::head ::y head-y]

     :then
     (tap> [:HEAD :position [head-x head-y]])]

    ::tail
    [:what
     [::tail ::x tail-x]
     [::tail ::y tail-y]

     :then
     (tap> [:TAIL :position [tail-x tail-y]])]


    ::map-boundaries
    [:what
     [::head ::x head-x]
     [::head ::y head-y]
     [::tail ::x tail-x]
     [::tail ::y tail-y]
     [::max ::y max-y {:then false}]
     [::min ::y min-y {:then false}]
     [::max ::x max-x {:then false}]
     [::min ::x min-x {:then false}]

     :then
     (o/insert! ::max {::y (max max-y head-y tail-y)
                       ::x (max max-x head-x tail-x)})
     (o/insert! ::min {::y (min min-y  head-y tail-y)
                       ::x (min min-x  head-x tail-x)})]

    ::shift-tail-north
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-north-of? [head-x head-y]
                   [tail-x tail-y])

     :then
     (tap> [:shift :tail :north])
     (o/insert! ::tail ::y (dec tail-y))]

    ::shift-tail-south
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-south-of? [head-x head-y]
                   [tail-x tail-y])

     :then
     (tap> [:shift :tail :south])
     (o/insert! ::tail ::y (inc tail-y))]

    ::shift-tail-west
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-west-of? [head-x head-y]
                  [tail-x tail-y])

     :then
     (tap> [:shift :tail :west])
     (o/insert! ::tail ::x (dec tail-x))]

    ::shift-tail-east
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-east-of? [head-x head-y]
                  [tail-x tail-y])

     :then
     (tap> [:shift :tail :east])
     (o/insert! ::tail ::x (inc tail-x))]

    ::shift-tail-northwest
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-northwest-of? [head-x head-y]
                       [tail-x tail-y])

     :then
     (tap> [:shift :tail :north-west])
     (o/insert! ::tail ::x (dec tail-x))
     (o/insert! ::tail ::y (dec tail-y))]

    ::shift-tail-northeast
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-northeast-of? [head-x head-y]
                       [tail-x tail-y])

     :then
     (tap> [:shift :tail :north-east])
     (o/insert! ::tail ::x (inc tail-x))
     (o/insert! ::tail ::y (dec tail-y))]

    ::shift-tail-southwest
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-southwest-of? [head-x head-y]
                       [tail-x tail-y])

     :then
     (tap> [:shift :tail :south-west])
     (o/insert! ::tail ::x (dec tail-x))
     (o/insert! ::tail ::y (inc tail-y))]

    ::shift-tail-southeast
    [:what
     [::head ::x head-x {:then not=}]
     [::head ::y head-y {:then not=}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :when
     (not (d9-h-and-t-valid? [head-x head-y]
                             [tail-x tail-y]))
     (d9-southeast-of? [head-x head-y]
                       [tail-x tail-y])

     :then
     (tap> [:shift :tail :south-east])
     (o/insert! ::tail ::x (inc tail-x))
     (o/insert! ::tail ::y (inc tail-y))]

    ::move-head-up
    [:what
     [::head ::x head-x {:then false}]
     [::head ::y head-y {:then false}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]
     [::move ::head ::up]

     :then
     (if-not (d9-h-and-t-valid? [head-x head-y]
                                [tail-x tail-y])
       (tap> [:move :rejected :up])
       (do
         (tap> [:move :head :up])
         (-> session
             (o/insert ::head ::y (dec head-y))
             (o/retract ::move ::head)
             (o/reset!))))]

    ::move-head-down
    [:what
     [::move ::head ::down]
     [::head ::x head-x {:then false}]
     [::head ::y head-y {:then false}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]

     :then
     (if-not (d9-h-and-t-valid? [head-x head-y]
                                [tail-x tail-y])
       (tap> [:move :rejected :down])
       (do
         (tap> [:move :head :down])
         (-> session
             (o/insert ::head ::y (inc head-y))
             (o/retract ::move ::head)
             (o/reset!))))]

    ::move-head-left
    [:what
     [::head ::x head-x {:then false}]
     [::head ::y head-y {:then false}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]
     [::move ::head ::left]

     :then
     (if-not (d9-h-and-t-valid? [head-x head-y]
                                [tail-x tail-y])
       (tap> [:move :rejected :left])
       (do
         (tap> [:move :head :left])
         (-> session
             (o/insert ::head ::x (dec head-x))
             (o/retract ::move ::head)
             (o/reset!))))]

    ::move-head-right
    [:what
     [::head ::x head-x {:then false}]
     [::head ::y head-y {:then false}]
     [::tail ::x tail-x {:then false}]
     [::tail ::y tail-y {:then false}]
     [::move ::head ::right]

     :then
     (if-not (d9-h-and-t-valid? [head-x head-y]
                                [tail-x tail-y])
       (tap> [:move :rejected :right])
       (do
         (tap> [:move :head :right])
         (-> session
             (o/insert ::head ::x (inc head-x))
             (o/retract ::move ::head)
             (o/reset!))))]

    ::keep-track-of-tail-positions
    [:what
     [::tail ::x tail-x]
     [::tail ::y tail-y]
     [::tail ::all-positions all-positions {:then false}]
     :then-finally
     (let [{:keys [tail-x
                   tail-y
                   all-positions]}
           (first (o/query-all session ::keep-track-of-tail-positions))]
       (tap> [:keeping :track :of :tail :position tail-x tail-y])
       (-> session
           (o/insert ::tail ::all-positions (set (conj all-positions [tail-x tail-y])))
           (o/reset!)))]

    ::print-position-map
    [:what
     [::head ::x head-x]
     [::head ::y head-y]
     [::tail ::x tail-x]
     [::tail ::y tail-y]
     [::max ::y max-y {:then false}]
     [::min ::y min-y {:then false}]
     [::max ::x max-x {:then false}]
     [::min ::x min-x {:then false}]

     :then-finally
     (let [[{:keys [head-x head-y
                    tail-x tail-y
                    max-x max-y
                    min-x min-y]} & _] (o/query-all session ::map-boundaries)
           valid? (d9-h-and-t-valid? [head-x head-y]
                                     [tail-x tail-y])
           map (apply str (->> (for [y (range (dec min-y) (+ 2 max-y))
                                     x (range (dec min-x) (+ 2 max-x))
                                     :let [eol? (= x (+ 1 max-x))]]
                                 (str (cond
                                        (= [x y] [head-x head-y])   \H
                                        (= [x y] [tail-x tail-y])   \T
                                        (= [x y] [0 0])             \s
                                        :else                       \.)
                                      (when eol? \newline)))))]
       (o/insert! ::map ::position map)
       (tap> (str "--------------------"
                  (str "Printing a position map -------------------------\n"
                       "Head: " head-x " " head-y "; Tail: " tail-x " " tail-y".\n"
                       (if valid? "Valid." "Invalid.") "\n"
                       map)
                  "--------------------")))]

    ::position-map
    [:what [::map ::position the-map]]}))



(defn d9-start-session
  [& {:keys [tail-x tail-y head-x head-y]}]
  (tap> ["------------------------------------"])
  (tap> [:starting :new :session ])
  (tap> ["------------------------------------"])
  (-> (reduce o/add-rule (o/->session) d9-rules)
      (o/insert ::tail ::x (or tail-x 0))
      (o/insert ::tail ::y (or tail-y 0))
      (o/insert ::head ::x (or head-x 0))
      (o/insert ::head ::y (or head-y 0))
      (o/insert ::max {::x 3 ::y 3})
      (o/insert ::min {::x -3 ::y -3})
      (o/insert ::tail ::all-positions #{})
      (o/fire-rules)))


(comment

  ::map-boundaries

  (-> (d9-start-session :head-x -2 :head-y 1
                        :tail-x 0 :tail-y 0)
      (o/query-all ::map-boundaries))


  (d9-h-and-t-valid? [2 0] [1 0])

  )

(defn d9-move-head
  [session direction]
  {:pre [(#{::right ::left ::up ::down} direction)]}
  (-> session
      (o/insert ::move ::head direction)
      (o/fire-rules)))

(defn d9-map-of-tail-locations
  [session]
  (let [[{:keys [all-positions]}]
        (o/query-all session
                     ::keep-track-of-tail-positions)

        [{:keys [head-x head-y
                 tail-x tail-y
                 max-x max-y
                 min-x min-y]}]
        (o/query-all session ::map-boundaries)]
    (apply str (for [y (range (dec min-y) (+ 2 max-y))
                     x (range (dec min-x) (+ 2 max-x))
                     :let [eol? (= x (+ 1 max-x))]]
                 (str (cond
                        (= [0 0] [x y])
                        \s

                        (all-positions [x y])
                        \#

                        :else
                        \.)
                      (when eol? \newline))))))

(comment

  (d9-northeast-of? [1 -2] [-1 -1])

  (let [r (-> (d9-start-session)
              (d9-move-head ::up)
              (d9-move-head ::up)
              (d9-move-head ::down)
              (d9-move-head ::right)
              (d9-move-head ::left)
              (d9-move-head ::right)
              (d9-move-head ::right)
              (d9-move-head ::down)
              (d9-move-head ::down))]
    (println "\n\n----------------------------------------\n\n")
    (println (:the-map (first (o/query-all r ::position-map))))
    (clojure.pprint/pprint [[::tail (first (o/query-all r ::tail))]
                            [::head (first (o/query-all r ::head))]
                            [::tail-positions (first (o/query-all r ::keep-track-of-tail-positions))]])
    (println (d9-map-of-tail-locations r))
    (flush))


  )

(def d9-test-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn d9-parse-input
  [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[d x]]
              (vector (case d
                        "U" ::up
                        "D" ::down
                        "L" ::left
                        "R" ::right)
                      (Integer/parseInt x))))))

(defn d9-make-head-executor
  [parsed]
  (->> (for [[direction nr] parsed
             _ (range nr)]
         #(d9-move-head % direction))
       (reverse)
       (apply comp)))

(defn d9-solve-1 [input]
  (let [parsed-input (d9-parse-input input)
        executor (d9-make-head-executor parsed-input)
        session (d9-start-session)
        result (executor session)
        visited-map (d9-map-of-tail-locations result)
        [{:keys [all-positions]}] (o/query-all result ::keep-track-of-tail-positions)]

    (println visited-map)
    (count all-positions)))

(comment

  (d9-solve-1 d9-test-input)
  (def d9-input (day-input-2022 9))

  (time (d9-solve-1 d9-input));; 5735

  (-> (d9-parse-input d9-test-input)
      (d9-make-head-instructions))

  )

(comment

  (add-tap (bound-fn* clojure.pprint/pprint))
  (remove-tap (bound-fn* clojure.pprint/pprint))

  (-> (d9-start-session)
      (o/insert ::move ::head ::right))

  (-> (d9-start-session))

  (-> (reduce o/add-rule (o/->session) rules)
      (o/insert ::tail ::x 0)
      (o/insert ::tail ::y 0)
      (o/insert ::head ::x 0)
      (o/insert ::head ::y 0)
      (o/fire-rules))

  (-> (reduce o/add-rule (o/->session) rules)
      (o/insert ::tail ::x 0)
      (o/insert ::tail ::y 0)
      (o/insert ::head ::x 1)
      (o/insert ::head ::y 0)
      (o/fire-rules))

  (-> (reduce o/add-rule (o/->session) rules)
      (o/insert ::tail ::x 0)
      (o/insert ::tail ::y 0)
      (o/insert ::head ::x 0)
      (o/insert ::head ::y -2)
      (o/fire-rules))



  (def session (atom (reduce o/add-rule (o/->session) rules)))



  (swap! session
         (fn [session]
           (-> session
               (o/insert ::tail ::x 1)
               (o/insert ::tail ::y 1)

               (o/insert ::head ::x 0)
               (o/insert ::head ::y 0)
               (o/fire-rules))))

  )

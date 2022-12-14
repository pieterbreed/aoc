(ns aoc
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [clojure.set :as set]))

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

(ns aoc
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as http]))

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

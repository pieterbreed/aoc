(ns aoc.2023.day4
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]))

(def example-input
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def table-row-re #"^Card\s+(\d+):([\s0-9]+)\|([\s0-9]+)$")

(comment

  (re-matches table-row-re
              "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")
  ["Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "2"
   " 13 32 20 16 61 "
   " 61 30 68 82 17 32 24 19"]


  )

(defn numbers-str->numbers-seq
  "Converts a string containing space-seperated numbers into a seq of numbers."
  [s]
  (as-> s $
    (str/trim $)
    (str/split $ #" ")
    (filter (comp pos? count) $)
    (map #(Long/parseLong %) $)))

(defn parse-input-table
  "Takes input table in string form, and returns the data inside it.
  Returns a seq of triplets. Each triplet has the form:
  [<row-number> <seq-of-winning-numbers> <seq-of-card-numbers>]"
  [input-table-str]
  (->> input-table-str
       (str/split-lines)
       (map (partial re-matches table-row-re))
       (map (partial drop 1))
       (map (fn [[row-nr winning-numbers-str card-numbers-str]]
              [(Long/parseLong row-nr)
               (numbers-str->numbers-seq winning-numbers-str)
               (numbers-str->numbers-seq card-numbers-str)]))))

(comment

  (parse-input-table example-input)

  )

(defn row->nrs-in-common
  "Counts how many numbers from the card in the winning numbers set."
  [[_ winning-nrs card-nrs]]
  (count (set/intersection (set winning-nrs)
                           (set card-nrs))))

(defn row->points
  "Converts the row data into a score for that row.

  The score is calculated by counting how many of the card-numbers are in the winning-numbers,
  then raising to to this power."
  [row-data]
  (let [nr-in-common (row->nrs-in-common row-data)]
    (long (math/pow 2 (dec nr-in-common)))))

(comment

  (row->points (nth (parse-input-table example-input) 5))

  )

(defn solution-1
  [input-table-str]
  (let [table-data (parse-input-table input-table-str)]
    (->> table-data
         (map row->points)
         (reduce +))))

(comment

  (solution-1 example-input) ;; 13
  (solution-1 (-common/day-input 2023 4))

  )

(defn solution-2
  [input-table-str]
  (let [table-data (parse-input-table input-table-str)
        ;; counts how many copies of each card we have
        card-counter (->> table-data
                          (map (juxt first (constantly 1)))
                          (into {}))
        card-counter' (reduce (fn [acc [card-nr & _ :as row-data]]
                                (let [card-score (row->nrs-in-common row-data)]
                                  (reduce (fn [acc' card-nr']
                                            (update acc'
                                                    card-nr'
                                                    (partial + (get acc card-nr))))
                                          acc
                                          (range (inc card-nr)
                                                 (+ card-nr card-score 1)))))
                              card-counter
                              table-data)]
    (reduce + (vals card-counter'))))

(comment

  (solution-2 example-input) ;; 30
  (solution-2 (-common/day-input 2023 4))

  )

(ns aoc
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

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

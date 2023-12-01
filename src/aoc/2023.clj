(ns aoc.2023
  (:require [aoc.common :as -common]
            [clojure.string :as str]))

(def input1 (-common/day-input 2023 1))

(def digit? (set (map (comp first str) (range 10))))

(defn first-and-last-digits
  [line-str]
  (let [digits (->> line-str
                    (seq)
                    (filter digit?))]
    [(str (first digits))
     (str (last digits))]))

(comment

  (first-and-last-digits "aoeu1oeu3oeu5")
  (first-and-last-digits "AOEU2aoeUaoeuaoeu3aoeuaoe3.423423")

  )

(defn calibration-document->calibration-values
  "Reads the calibration document, scans each line, retrieves the calibration value for that line.

  This is the long value, of the first and last digit combined.

  Returns a seq of these longs."
  [input-document]
  (->> input-document
       (str/split-lines)
       (map #(apply str (first-and-last-digits %)))
       (map #(Long/parseLong %))))

(def test-input-1
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(comment

  (calibration-document->calibration-values test-input-1)

  )

(defn solution-1
  "Sums up all of the calibration values of a calibration document."
  [calibration-doc-str]
  (->> calibration-doc-str
       (calibration-document->calibration-values)
       (reduce +)))

(comment

  (solution-1 test-input-1)

  (solution-1 (-common/day-input 2023 1))

  )

(def calibration-values->numeric-values
  {"one"   1 "1" 1
   "two"   2 "2" 2
   "three" 3 "3" 3
   "four"  4 "4" 4
   "five"  5 "5" 5
   "six"   6 "6" 6
   "seven" 7 "7" 7
   "eight" 8 "8" 8
   "nine"  9 "9" 9})

(defn first-occurance-of-any
  "Takes a string (haystack), and a set of strings (needles).

  Returns the first occurance of any of the needles in the haystack."
  [haystack needles]
  (loop [remainder haystack]
    (let [needle-found? (->> needles
                             (filter #(str/starts-with? remainder %))
                             (first))]
      (if needle-found?
        needle-found?
        (recur (subs remainder 1))))))

(comment

  (first-occurance-of-any "aaothreeeuoe3uaoeuaoeuone" (keys calibration-values->numeric-values)) ;; "three"
  (first-occurance-of-any "a2aothreeeuoe3uaoeuaoeuone" (keys calibration-values->numeric-values)) ;; "2"

  )

(defn first-and-last-number-value
  "Takes the first and last occurance of any of the keys in `calibration-values->numeric-values` and returns in a tuple."
  [line-str]
  (let [first-value (first-occurance-of-any line-str
                                            (keys calibration-values->numeric-values))
        last-value (str/reverse
                    (first-occurance-of-any (str/reverse line-str)
                                            (map str/reverse (keys calibration-values->numeric-values))))]
    [(str (calibration-values->numeric-values first-value))
     (str (calibration-values->numeric-values last-value))]))

(comment

  (first-and-last-number-value "two1nine") ;; ["2" "9"]
  (first-and-last-number-value "eightwothree") ;; ["8" "3"]
  (first-and-last-number-value "abcone2threexyz") ;; ["1" "3"]

  )

(defn calibration-document->calibration-values2
  "Reads the calibration document, scans each line, retrieves the calibration value for that line.

  Calibration values may be any of the keys in `calibration-values->numeric-values`.

  The first and the last calibration values will have their numeric values taken and combined.

  This is a long value, which is returned in a sequence, one for each line."
  [input-document]
  (->> input-document
       (str/split-lines)
       (map #(apply str (first-and-last-number-value %)))
       (map #(Long/parseLong %))))

(defn solution-2
  "Sums up all of the calibration values of a calibration document."
  [calibration-doc-str]
  (->> calibration-doc-str
       (calibration-document->calibration-values2)
       (reduce +)))

(def test-input-2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(comment

  (solution-2 test-input-2) ;; 281
  (solution-2 input1)

  )

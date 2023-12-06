(ns aoc.2023.day6
  (:require [aoc.common :as -common]
            [clojure.string :as str]))

(def example-input
  "Time:      7  15   30
Distance:  9  40  200")

(defn parse-line
  [l]
  (as-> l $
    (str/split $ #":")
    (second $)
    (str/trim $)
    (str/split $ #" ")
    (filter (comp pos? count) $)
    (map #(Long/parseLong %) $)))

(comment

  (parse-line "Time:      7  15   30")
  (parse-line "Distance:  9  40  200")
  )


(defn parse-input
  [input]
  (let [[time-line distance-line & _] (str/split-lines input)
        times (parse-line time-line)
        distances (parse-line distance-line)
        times-and-distances (map vector times distances)]
    times-and-distances))

(comment

  (parse-input example-input)

  )

(defn wins-with-button-time?
  "Presses the button for `button-time` (equals the speed of the boat),
  calculates how far the boat goes in remaining time,
  returns true if this is further than the record distance."
  [race-time record-distance button-time]
  (let [speed button-time
        travel-time (- race-time button-time)
        distance (* speed travel-time)]
    (< record-distance distance)))

(defn count-winnable-button-times
  "Determines how many winnable button-times there are for a race and record distance."
  [race-time record-distance]
  (let [first-winning-button-time (->> (range (inc race-time))
                                       (filter (comp true? (partial wins-with-button-time? race-time record-distance)))
                                       (first))
        last-winning-button-time (->> (range (inc race-time) 0 -1)
                                      (filter (comp true? (partial wins-with-button-time? race-time record-distance)))
                                      (first))]
    (inc (- last-winning-button-time
            first-winning-button-time))))

(comment

  (for [i (range (inc 7))]
    [i (wins-with-button-time? 7 9 i)])

  )

(defn solution-1
  [input]
  (let [race-data (parse-input input)]
    (reduce * (map #(apply count-winnable-button-times %)
                   race-data))))

(comment

  (solution-1 example-input) ;; 288
  (solution-1 (-common/day-input 2023 6))

  )

(ns aoc.2023.day9
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.core.async :as csp]))

(def example-input
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [l] (map #(Long/parseLong %) l)))))

(comment

  (parse-input example-input)

  )

(defn next-line
  [[_ & line-rest :as line]]
  (map #(- %2 %1)
       line
       line-rest))

(defn last-line?
  [line]
  (apply = 0 line))

(comment

  (next-line [0 3 6 9 12 15])
  (-> [1 3 6 10 15 21]
      next-line
      next-line
      next-line
      last-line?)

  (next-line [10 13 16 21 30 45])

  )

(defn predict-next-value
  [line]
  (if (last-line? line)
    0
    (let [next-line (next-line line)
          current-last (last line)
          next-line-predicted-value (predict-next-value next-line)]
      (+ current-last next-line-predicted-value))))
(comment

  (predict-next-value [0 3 6 9 12 15]) ;; 18
  (predict-next-value [1 3 6 10 15 21]) ;; 28
  (predict-next-value [10 13 16 21 60 45]) ;; -382 ???
  (predict-next-value [10 13 16 21 30 45]) ;; 68

  )

(defn solution-1
  [input]
  (let [input-data (parse-input input)
        r (sequence (comp (map predict-next-value)
                          (x/reduce +))
                    input-data)]
    (first r)))

(comment

  (solution-1 example-input)
  (solution-1 (-common/day-input 2023 9))

  )

(defn predict-prev-value
  [line]
  (if (last-line? line)
    0
    (let [next-line (next-line line)
          current-first (first line)
          next-line-predicted-value (predict-prev-value next-line)]
      (- current-first
         next-line-predicted-value))))

(comment

  (predict-prev-value [10 13 16 21 30 45])

  )

(defn solution-2
  [input]
  (let [input-data (parse-input input)
        r (sequence (comp (map predict-prev-value)
                          (x/reduce +))
                    input-data)]
    (first r)))

(comment

  (solution-2 example-input) ;; 2
  (solution-2 (-common/day-input 2023 9))

  )

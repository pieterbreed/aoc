(ns aoc.2025.day3
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   ))

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map (fn [s]
              (into []
                    (map (comp Long/parseLong str))
                    s)))))

(def test-input
  "987654321111111
811111111111119
234234234234278
818181911112111")

(comment

  (parse-input test-input)


  )

(defn max-joltage-per-bank-1
  [xs]
  (let [initial-bank (butlast xs)
        max-1        (apply max initial-bank)
        max-1-i (loop [[hd & rst] initial-bank
                       i          0]
                  (if (= hd max-1)
                    i
                    (recur rst (inc i))))
        rest-bank (drop (inc max-1-i) xs)
        rest-max (apply max rest-bank)]
    (+ (* 10 max-1)
       rest-max)))

(defn solve-1
  [input]
  (let [p (parse-input input)
        joltages (->> p
                      (map max-joltage-per-bank-1)
                      (reduce +))]
    joltages))

(comment

  (solve-1 test-input) ;; 357
  (solve-1 (-common/day-input 2025 3))

  )

(defn max-at-index
  "Determines the index i at which the max of xs can be found"
  [xs]
  (->> xs
       (map-indexed vector)
       (reduce (fn [[_max-i max-x :as cur] [_i x :as nex]]
                 (if (< max-x x)
                   nex
                   cur)))))

(defn max-joltage-per-bank-2
  [xs nr-batteries]
  (let [max-joltage-nrs
        (loop [xss          xs
               nr-remaining nr-batteries
               acc          []]


          (if-not (pos? nr-remaining)
            acc
            (let [search-space (drop-last (dec nr-remaining)
                                          xss)
                  [max-i max-x] (max-at-index search-space)]
              (recur (drop (inc max-i) xss)
                     (dec nr-remaining)
                     (conj acc max-x)))))]

    (->> max-joltage-nrs
         (reverse)
         (map-indexed vector)
         (reduce (fn [acc [i x]]
                   (+ acc (* x (long (math/pow 10 i)))))
                 0))))

(comment

  (max-joltage-per-bank-2 [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1] 12) ;; 987654321111
  (max-joltage-per-bank-2 [8 1 1 1 1 1 1 1 1 1 1 1 1 1 9] 12) ;; 811111111119
  (max-joltage-per-bank-2 [2 3 4 2 3 4 2 3 4 2 3 4 2 7 8] 12) ;; 434234234278
  (max-joltage-per-bank-2 [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1] 12) ;; 888911112111

  )

(defn solve-2
  [input]
  (let [input (parse-input input)]
    (->> input
         (map #(max-joltage-per-bank-2 % 12))
         (reduce +))))

(comment

  (solve-2 test-input) ;; 3121910778619
  (solve-2 (-common/day-input 2025 3)) ;;

  )

(ns aoc.2024.day2
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))


(def input-1 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map (fn [l] (->> (str/split l -common/whitespace-regex)
                         (map Long/parseLong))))))

(comment

  (parse-input input-1)

  )

(defn gradually?
  [report]
  (->> (partition 2 1 report)
       (every? (fn [[a b]]
                 (let [d (abs (- a b))]
                   (<= 1 d 3))))))

(defn decreasing?
  [report]
  (->> (partition 2 1 report)
       (every? (fn [[a b]] (> a b)))))

(defn increasing?
  [report]
  (->> (partition 2 1 report)
       (every? (fn [[a b]] (< a b)))))

(defn safe-report?
  [report]
  (or ((every-pred gradually? decreasing?) report)
      ((every-pred gradually? increasing?) report)))

(comment

  (decreasing? (second (parse-input input-1)))
  (gradually? (first (parse-input input-1)))
  (gradually? (second (parse-input input-1)))

  )

(defn sol1
  [input]
  (->> (parse-input input)
       (filter safe-report?)
       (count)))

(comment

  (sol1 input-1)
  (sol1 (-common/day-input 2024 2))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-report2?
  [report]
  (let [cases (concat [report]
                      (for [i (range (count report))
                            :let [[a b] (split-at i report)]]
                        (concat a (rest b))))]
    (some #(or ((every-pred gradually? decreasing?) %)
               ((every-pred gradually? increasing?) %))
          cases)))

(defn sol2
  [input]
  (->> (parse-input input)
       (filter safe-report2?)
       (count)))

(comment

  (sol2 input-1)
  (sol2 (-common/day-input 2024 2))

  )

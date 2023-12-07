(ns aoc.2023.day7
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]))

(def example-input
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map str/trim)
       (map #(str/split % #" "))
       (map (fn [[hand bid]]
              [hand (Long/parseLong bid)]))))

(comment

  (parse-input example-input)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def card-labels (vec (reverse [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2 \1])))

(defn compare-card
  "Accepts an operator (example `<`) and returns a function that compares two cards based on this operator."
  [a b]
  (compare (.indexOf card-labels a)
           (.indexOf card-labels b)))

(comment

  (sort compare-card "32AT1J876")

  )

(def type-order (vec (reverse [:five-kind :four-kind :full-house :three-kind :two-pair :one-pair :high-card])))

(defn compare-type
  [a b]
  (compare (.indexOf type-order a)
           (.indexOf type-order b)))

(defn hand->type
  "Takes a hand, and determines the type.
  One of: :five-kind :four-kind :full-house :three-kind :two-pair :one-pair :high-card"
  [hand]
  (let [[highest next-highest & _]  (into []
                                          (comp (x/by-key identity x/count)
                                                (map second)
                                                (x/sort-by identity >))
                                          hand)]
    (cond
      (= highest 5)              :five-kind
      (= highest 4)              :four-kind
      (and (= highest 3)
           (= next-highest 2))   :full-house
      (and (= highest 3)
           (= next-highest 1))   :three-kind
      (= 2 highest next-highest) :two-pair
      (and (= 2 highest)
           (= 1 next-highest))   :one-pair
      (= 1 highest)              :high-card)))

(defn compare-hand:type
  [a b]
  (compare-type (hand->type a)
                (hand->type b)))

(defn compare-hand:tie-break
  [[a1 & arest :as a] [b1 & brest :as b]]
  (cond
    (= a b)            0
    (not= a1 b1)       (compare-card a1 b1)
    :else              (recur arest brest)))

(defn compare-hand
  [a b]
  (let [x (compare-hand:type a b)]
    (if-not (zero? x)
      x
      (compare-hand:tie-break a b))))

(comment

  (sort compare-hand
        ["32T3K"
         "T55J5"
         "KK677"
         "KTJJT"
         "QQQJA"])

  )

(defn solution-1
  [input]
  (->> input
       (parse-input)
       (sort-by first
                compare-hand)
       (map-indexed (fn [i [_ bid]] (* (inc i) bid)))
       (reduce +)))

(comment

  (solution-1 example-input) ;; 6440
  (solution-1 (-common/day-input 2023 7))

  )

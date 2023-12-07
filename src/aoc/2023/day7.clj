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

(def card-labels-2 (vec (reverse [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \1 \J])))

(defn compare-card-2
  [a b]
  (compare (.indexOf card-labels-2 a)
           (.indexOf card-labels-2 b)))



(defn hand->type-2
  "Takes a hand, and determines the type.
  One of: :five-kind :four-kind :full-house :three-kind :two-pair :one-pair :high-card.
  Allows J cards to be jokers to match the best outcome."
  [hand]
  (let [{nr-jokers \J
         :as       card-counts} (into {}
                          (x/by-key identity x/count)
                          hand)
        [highest next-highest & _]  (into []
                                          (comp (map second)
                                                (x/sort-by identity >))
                                          (dissoc card-counts \J))
        highest' (+ (or highest 0)
                    (or nr-jokers 0))]
    (cond
      (= highest' 5)              :five-kind
      (= highest' 4)              :four-kind
      (and (= highest' 3)
           (= next-highest 2))    :full-house
      (and (= highest' 3)
           (= next-highest 1))    :three-kind
      (= 2 highest' next-highest) :two-pair
      (and (= 2 highest')
           (= 1 next-highest))    :one-pair
      (= 1 highest')              :high-card)))

(comment

  (hand->type-2 "32T3K")
  (hand->type-2 "T55J5")
  (hand->type-2 "KK677")
  (hand->type-2 "KTJJT")
  (hand->type-2 "QQQJA")


  )

(defn compare-hand:type-2
  [a b]
  (compare-type (hand->type-2 a)
                (hand->type-2 b)))

(defn compare-hand:tie-break-2
  [[a1 & arest :as a] [b1 & brest :as b]]
  (cond
    (= a b)            0
    (not= a1 b1)       (compare-card-2 a1 b1)
    :else              (recur arest brest)))

(defn compare-hand-2
  [a b]
  (let [x (compare-hand:type-2 a b)]
    (if-not (zero? x)
      x
      (compare-hand:tie-break-2 a b))))

(defn solution-2
  [input]
  (->> input
       (parse-input)
       (sort-by first
                compare-hand-2)
       (map-indexed (fn [i [_ bid]] (* (inc i) bid)))
       (reduce +)))

(comment

  (solution-2 example-input) ;; 5905
  (solution-2 (-common/day-input 2023 7))

  )

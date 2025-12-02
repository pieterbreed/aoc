(ns aoc.2025.day2
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   ))

(defn parse-input
  [s]
  (let [ranges (str/split s #",")
        ranges' (->> ranges
                     (map str/trim)
                     (map #(str/split % #"-"))
                     (map (fn [[a b]]
                            [(Long/parseLong a)
                             (Long/parseLong b)])))]
    ranges'))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(comment

  (parse-input test-input)
  (println (-common/day-input 2025 2))

  )

(defn is-repeated-nr?
  [n]
  ;; b10 must be odd, for the number to have an even number of digits
  ;; and only numbers, with even numbers of digits may be repeating
  (let [b10 (-> (math/log10 n)
                (math/floor)
                (long))]
    (when (odd? b10)
      (let [lower-power (long (math/pow 10
                                        (-> b10
                                            (inc)
                                            (/ 2))))
            lower-numbers (mod n lower-power)
            higher-numbers (quot n lower-power)]

        (= lower-numbers
           higher-numbers)))))

(comment

  (is-repeated-nr? 11)
  (is-repeated-nr? 22)
  (is-repeated-nr? 101)
  (is-repeated-nr? 1010)
  (is-repeated-nr? 1188511885)

  )

(defn solve-1
  [input]
  (let [ranges (parse-input input)
        invalid-numbers (->> (for [[l h] ranges
                                   i     (range l (inc h))
                                   :when (is-repeated-nr? i)]
                               i))]
    (reduce + invalid-numbers)))

(comment

  (solve-1 test-input) ;; 1227775554
  (solve-1 (-common/day-input 2025 2)) ;;

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (->> (parse-input (-common/day-input 2025 2))
       (map second)
       (reduce max))
  ;; 9875208883
  (-> (math/log10 9875208883)
      (math/floor)
      (long))
  ;; 9

  )

;; what sizes of groupings are possible for the _size_ of the number (power of 10)
(def group-sizes {2  [1]
                  3  [1]
                  4  [2 1]
                  5  [1]
                  6  [3 2 1]
                  7  [1]
                  8  [4 2 1]
                  9  [3 1]
                  10 [5 2 1]})

(defn is-repeated-nr-2?
  [n]
  (let [b10 (-> (math/log10 n)
                (math/floor)
                (long)
                (inc))
        groups (for [size (get group-sizes b10)
                     :let [x (math/pow 10 size)]]
                 (loop [r n
                        groups []]
                   (if (< r 1)
                     groups
                     (recur (long (quot r x))
                            (conj groups
                                  (long (mod r x)))))))]
    (some (fn [xs] (apply = xs))
          groups)))

(comment

  (for [x [11 22 99 111
           999 1010
           1188511885
           222222
           446446
           38593859
           565656
           824824824
           2121212121]]
    [x (is-repeated-nr-2? x)])

  )

(defn solve-2
  [input]
  (let [ranges (parse-input input)
        invalid-numbers (->> (for [[l h] ranges
                                   i     (range l (inc h))
                                   :when (is-repeated-nr-2? i)]
                               i))]
    (reduce + invalid-numbers)))

(comment

  (solve-2 test-input) ;; 4174379265
  (solve-2 (-common/day-input 2025 2)) ;;


  )

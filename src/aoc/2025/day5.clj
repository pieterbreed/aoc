(ns aoc.2025.day5
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   ))

(defn parse-1
  [input]
  (let [input'                         (str/split-lines input)
        [fresh-list' [_ & ingredients']] (split-with (comp pos? count) input')
        fresh-list (->> fresh-list'
                        (map #(str/split % #"-"))
                        (map (fn [[a b]] [(Long/parseLong a) (Long/parseLong b)])))
        ingredients (->> ingredients'
                         (map Long/parseLong))]
    [fresh-list ingredients]))

(def test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(comment

  (parse-1 test-input)

  )

(defn solve-1
  [input]
  (let [[fl ing] (parse-1 input)
        is-fresh? (fn [x]
                    (some (fn [[l u]] (<= l x u))
                          fl))]
    (->> ing
         (filter is-fresh?)
         (count))))

(comment

  (solve-1 test-input) ;; 3
  (solve-1 (-common/day-input 2025 5)) ;; 770

  )

(defn solve-2
  [input]
  (let [[fl _] (parse-1 input)]
    (count (reduce (fn [fresh-ingredient-ids-set [l up]]
                     (into fresh-ingredient-ids-set (range l (inc up))))
                   #{}
                   fl))))

(comment

  (solve-2 test-input) ;; 14
  (solve-2 (-common/day-input 2025 5)) ;; crashes the heap

  )

(defn overlap-with-ranges?
  [existing-ranges [a b :as _new-range]]
  (filter (fn [[erl eru]]
            (or (<= erl a eru)
                (<= erl b eru)
                (<= a erl b)
                (<= a eru b)))
          existing-ranges))

(defn merge-overlapping-range
  [[l u :as _existng-range]
   [a b :as _overlapping-range]]
  (when-not (or (<= l a u)
                (<= l b u)
                (<= a l b)
                (<= a u b))
    (throw (ex-info "expectations unmet" {:l l :u u :a a :b b})))
  [(min l a)
   (max u b)])

(comment

  (overlap-with-ranges? [[3 5] [10 14] [16 20]] [12 18])
  (merge-overlapping-range [10 14] [12 18])


  )

(defn solve-2-2
  [input]
  (let [[fl _] (parse-1 input)
        merged-ranges
        (reduce (fn [acc r]
                  (let [overlapping-ranges (overlap-with-ranges? acc r)]
                    (if-not (seq overlapping-ranges)
                      (conj acc r)
                      (let [rem (set/difference acc (set overlapping-ranges))
                            combined-range (reduce merge-overlapping-range
                                                   r
                                                   overlapping-ranges)]
                        (conj rem combined-range)))))
                #{}
                fl)]
    (->> merged-ranges
         (map (fn [[l u]] (inc (- u l))))
         (reduce +))))

(comment

  (solve-2-2 test-input) ;; 14
  (solve-2-2 (-common/day-input 2025 5))
  (println (-common/day-input 2025 5))

  )

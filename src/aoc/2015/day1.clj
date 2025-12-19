(ns aoc.2015.day1
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))

(def test-input-1 "(())")

(defn solve-1
  [input]
  (let [{up \(
         down \)} (frequencies input)]
    (- up down)))

(comment

  (solve-1 test-input-1) ;; {\( 2, \) 2}
  (solve-1 test-input-1) ;; 0
  (solve-1 (-common/day-input 2015 1)) ;; 138

  )

(defn solve-2
  [input]

  (->> input
       (map-indexed vector)
       (reduce (fn [s [i x]]
                 (let [r
                       (if (= \( x)
                         (inc s)
                         (dec s))]
                   (if (< r 0)
                     (reduced i)
                     r)))
               0)
       (inc)))

(comment

  (solve-2 "()())") ;; 5
  (solve-2 (-common/day-input 2015 1)) ;;

  )

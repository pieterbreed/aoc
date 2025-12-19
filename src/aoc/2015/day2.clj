(ns aoc.2015.day2
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #"x"))))

(comment

  (parse-input "2x3x6\n4x5x6")


  )

(def chars->long (comp Long/parseLong (fn [& x] (apply str x))))

(def p (ip/parser "
input = input-line (<'\n'> input-line)+
input-line = l <'x'> w <'x'> h
l = dim
w = dim
h = dim
<dim> = #\"\\d\"+"))

(defn parse-input'
  [input]
  (ip/transform {:l          chars->long
                 :h          chars->long
                 :w          chars->long
                 :input-line vector
                 :input      (fn [& x] (vec x))}
                (ip/parse p (str/trim input))))

(comment

  (parse-input' "2x3x3\n5x6x7\n")
  (ip/parse p "2x3x3\n5x6x7")


  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn how-much-wrapping-paper
  [[l w h :as dims]]
  (+ (* 2 l w)
     (* 2 l h)
     (* 2 w h)
     (apply * (take 2 (sort dims)))))

(comment

  (how-much-wrapping-paper [2 3 4]) ;; 58
  (how-much-wrapping-paper [1 1 10]) ;; 43

  )

(defn solve-1
  [input]
  (let [input (parse-input' input)]
    (reduce + (map how-much-wrapping-paper input))))

(comment

  (parse-input' (-common/day-input 2015 2))
  (solve-1 (-common/day-input 2015 2)) ;; 1586300

  (println (-common/day-input 2015 2))

  )

(defn how-much-ribbon
  [[l w h :as dims]]
  (let [m (min (+ l l w w)
               (+ l l h h)
               (+ w w h h))
        volume (* l w h)]
    (+ volume m)))

(defn solve-2
  [input]
  (let [input (parse-input' input)]
    (->> input
         (map how-much-ribbon)
         (reduce +))))

(comment

  (solve-2 (-common/day-input 2015 2)) ;;

  )

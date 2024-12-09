(ns aoc.2024.day3
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))


(def input-1
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def mul-instruction-re #"mul\((\d{1,3}),(\d{1,3})\)")

(comment

  (re-matches mul-instruction-re "mul(133,345)")
  (re-matches mul-instruction-re "mul ( 2 , 4 )")

  (re-seq mul-instruction-re input-1)

  )

(defn find-muls
  [input]
  (->> (re-seq mul-instruction-re input)
       (map (fn [[_ a b]]
              [(Long/parseLong a)
               (Long/parseLong b)]))))

(comment

  (find-muls input-1)

  )

(defn sol1
  [input]
  (let [ms (find-muls input)]
    (reduce + (map #(apply * %) ms))))

(comment

  (sol1 input-1)
  (sol1 (-common/day-input 2024 3)) ;;

  )

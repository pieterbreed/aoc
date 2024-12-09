(ns aoc.2024.day3
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))


(def input-1
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def mul-instruction-1-re #"mul\((\d{1,3}),(\d{1,3})\)")

(comment

  (re-matches mul-instruction-1-re "mul(133,345)")
  (re-matches mul-instruction-1-re "mul ( 2 , 4 )")

  (re-seq mul-instruction-1-re input-1)

  )

(defn find-muls
  [input]
  (->> (re-seq mul-instruction-1-re input)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(def mul-instruction-2-re #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)")


(defn do-op
  [{:as   state
    :keys [doing?]}
   [op-name & op-params]]
  (case op-name
    :mul (if doing?
           (update state
                   :running-total
                   (fnil #(+ % (let [[a b] op-params]
                                 (* a b)))
                         0))
           state)
    :do    (assoc state :doing? true)
    :don't (assoc state :doing? false)))

(comment

  (re-seq mul-instruction-2-re input-2)


  )

(defn find-muls-2
  [input]
  (->> (re-seq mul-instruction-2-re input)
       (map (fn [[x & ps]]
              (cond
                (= "mul" (subs x 0 3)) [:mul (Long/parseLong (first ps)) (Long/parseLong (second ps))]
                (= "do()" x)         [:do]
                (= "don't()" x)      [:don't])))
       (reduce do-op {:doing? true})
       :running-total))

(comment

  (find-muls-2 input-2) ;; 48
  (find-muls-2 (-common/day-input 2024 3)) ;;

  )

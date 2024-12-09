(ns aoc.2024.day1
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))


(def test-input-1
  "3   4
4   3
2   5
1   3
3   9
3   3")

(defn input->lists
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #"\p{Zs}+"))
       (map #(map Integer/parseInt %))))

(defn lists->list
  [lists i]
  (map #(nth % i) lists))

(comment

  (-> test-input-1
      (input->lists)
      #_(lists->list 0))

  )

(defn sol1
  [input]
  (let [ll (input->lists input)
        l1 (lists->list ll 0)
        l2 (lists->list ll 1)]
    (->> (map vector (sort l1) (sort l2))
         (map #(apply - %))
         (map abs)
         (reduce +))))


(comment

  (sol1 test-input-1)
  (sol1 (-common/day-input 2024 1))

  )

(defn sol2
  [input]
  (let [ll (input->lists input)
        l1 (lists->list ll 0)
        l2 (lists->list ll 1)
        s2-fr (frequencies l2)]

    (->> l1
         (map #(* % (get s2-fr % 0)))
         (reduce +))))

(comment

  (sol2 test-input-1)
  (sol2 (-common/day-input 2024 1))

  )

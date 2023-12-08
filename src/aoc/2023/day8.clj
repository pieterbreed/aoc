(ns aoc.2023.day8
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]))


(def example-input-1
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def example-input-2
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def network-line-re #"([^\s]+)\s+=\s+\(([^,]+),\s*([^\)]+)\)")

(comment

  (re-matches network-line-re "AAA = (BBB, CCC)")
  (re-matches network-line-re "BBB = (DDD, EEE)")
  (re-matches network-line-re "CCC = (ZZZ, GGG)")

  )

(defn parse-input
  [input]
  (let [[instructions _ & network-lines] (str/split-lines input)
        instructions' (->> instructions
                           (str/trim)
                           (cycle))
        network-def (->> network-lines
                         (map #(re-matches network-line-re %))
                         (map (juxt second #(vector (nth % 2) (nth % 3))))
                         (into {}))]
    {:instructions instructions'
     :network      network-def}))

(comment

  (parse-input example-input-1)

  )

(defn solution-1
  [input]
  (let [{:keys [instructions
                network]}    (parse-input input)]
    (loop [steps 1
           current-node "AAA"
           [next-nav & instructions'] instructions]
      (let [next-node (let [[l-node r-node] (get network current-node)]
                        (case next-nav
                          \L l-node
                          \R r-node))]
        (if (= "ZZZ" next-node)
          steps
          (recur (inc steps)
                 next-node
                 instructions'))))))

(comment

  (solution-1 example-input-1) ;; 2
  (solution-1 example-input-2) ;; 6
  (solution-1 (-common/day-input 2023 8))

  )

(defn solution-2
  [input]
  (let [{:keys [instructions
                network]}    (parse-input input)
        lookup-step (fn [nav node]
                      (let [[l-node r-node] (get network node)]
                        (case nav
                          \L l-node
                          \R r-node)))]
    (loop [steps 1
           current-nodes (->> network
                              (keys)
                              (filter #(str/ends-with? % "A"))
                              (vec))
           [next-nav & instructions'] instructions]
      (let [next-nodes (->> current-nodes
                            (mapv (partial lookup-step next-nav)))
            all-arrived? (every? #(str/ends-with? % "Z") next-nodes)]
        (if all-arrived?
          steps
          (recur (inc steps)
                 next-nodes
                 instructions'))))))

(def example-input-3
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(comment

  (solution-2 example-input-3) ;; 6
  (solution-2 (-common/day-input 2023 8))

  )

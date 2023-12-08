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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-ghost-pattern
  "Returns an infinite list of steps this ghost will have taken to reach the ending position."
  [{:keys [network
           instructions]}
   starting-position]
  (let [lookup-next-node (fn [node nav]
                           (let [[l-node r-node] (get network node)]
                             (case nav
                               \L l-node
                               \R r-node)))
        ending-node? #(str/ends-with? % "Z")

        [initial-arrival-step ending-node instructions*]
        (loop [steps 0
               current-node starting-position
               [next-nav & instructions' :as instructions*] instructions]
          (if (ending-node? current-node)
            [steps current-node instructions*]
            (recur (inc steps)
                   (lookup-next-node current-node next-nav)
                   instructions')))

        next-arrival-step
        (loop [steps (inc initial-arrival-step)
               current-node (lookup-next-node ending-node (first instructions*))
               [next-nav & instructions''] (rest instructions*)]
          (if (ending-node? current-node)
            steps
            (recur (inc steps)
                   (lookup-next-node current-node next-nav)
                   instructions'')))]
    {:starting-node   starting-position
     :initial-arrival initial-arrival-step
     :next-arrival    next-arrival-step
     :cycle           (- next-arrival-step initial-arrival-step)
     :ending-node     ending-node}))


(comment

  (let [input-data (parse-input example-input-3)]
    [(find-ghost-pattern input-data "22A")
     (find-ghost-pattern input-data "22A")])

  )

(defn ghost-pattern->inf-steps
  [{:keys [initial-arrival
           cycle]}]
  (->> (range)
       (map #(+ initial-arrival
                (* % cycle)))))

(comment

  (let [input-data (parse-input example-input-3)]
    [(take 10 (ghost-pattern->inf-steps (find-ghost-pattern input-data "11A")))
     (take 10 (ghost-pattern->inf-steps (find-ghost-pattern input-data "22A")))])

  (let [input-data (parse-input (-common/day-input 2023 8))
        starting-ghosts (->> input-data
                               :network
                               keys
                               (filterv #(str/ends-with? % "Z")))]
    [(take 10 (ghost-pattern->inf-steps (find-ghost-pattern input-data (first starting-ghosts))))
     (take 10 (ghost-pattern->inf-steps (find-ghost-pattern input-data (second starting-ghosts))))])





  )

(defn solution-2-2
  "Computes the repeating pattern of every ghost,
  then walks the pattern for every ghost concurrently, carefully keaping track of the current-max step number,
  until we find the step number on which they all arrive at at once.

  Some ghosts will have slow-moving but huge size incements.
  Some ghosts will have short tight patterns.

  The pattern is like this:
  on step 1, ghost 1 start at his starting position, g-1-a
  on step g-i, ghost 1 arrives at his ending-position g-1-z
  on step g-j, ghost 1 arrives at his ending-position g-1-z, again.
  (- g-j g-i) is the cycle size, how many steps to come back to ending position.
  The pattern of valid steps, for every ghost n is then:
  [g-n-i  (+ g-n-j
             (* 0 (- g-n-j
                          g-n-i)))
          (+ g-n-j
             (* 1 (- g-n-j
                     g-n-i)))

          (+ g-n-j
             (* 2 (- g-n-j
                     g-n-i)))

          (+ g-n-j
             (* ... (- g-n-j
                       g-n-i)))]"

  [input]
  (let [{:keys [network]
         :as input-data}    (parse-input input)
        all-ghosts (->> network
                        (keys)
                        (filterv #(str/ends-with? % "A")))
        all-ghost-steps (map (comp ghost-pattern->inf-steps
                                   (partial find-ghost-pattern input-data))
                             all-ghosts)]
    (loop [all-ghost-steps' all-ghost-steps]
      (let [current-steps (map first all-ghost-steps')
            current-max-ghost-step (reduce max current-steps)
            all-at-max? (every? (partial = current-max-ghost-step)
                                current-steps)]
        (if all-at-max?
          current-max-ghost-step
          (recur (mapv (partial drop-while #(< % current-max-ghost-step))
                       all-ghost-steps')))))))

(comment

  (solution-2-2 example-input-3) ;; 6
  (solution-2-2 (-common/day-input 2023 8))



  )

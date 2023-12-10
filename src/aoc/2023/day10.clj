(ns aoc.2023.day10
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.core.async :as csp]))


(def example-input-1
  ".....
.S-7.
.|.|.
.L-J.
.....")

(def example-input-2
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def example-input-3
  "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(def example-input-4
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(defn parse-map
  "Reads the map and returns a map.
  This map contains:
  - :S -> [x y] of starting position
  - :lookup -> (fn ^char lookup [x y])"
  [input]
  (let [lines (str/split-lines input)]
    {:S (->> lines
             (map-indexed (fn [i x] [(str/index-of x "S") i]))
             (filter (comp some? first))
             (first))
     :size [(count (first lines))
            (count lines)]
     :lookup
     (fn [x y]
       (get-in lines [y x]))}))

(comment


  (parse-map example-input-1)
  (parse-map example-input-3)

  (let [{:keys [lookup S]} (parse-map example-input-4)]
    (apply lookup S))

  )

(defn follow-segment
  "Takes the map data, current x and y, and which direction (:north, :south, :west, :east) entering the current x and y from."
  [{:keys [lookup] :as map-data}
   {:keys [current-x current-y enter-from] :as state}]
  (when state
    (let [segment (lookup current-x current-y)]
      (enter-from (case segment
                    \| {:south {:current-x current-x       :current-y (dec current-y) :enter-from :south}
                        :north {:current-x current-x       :current-y (inc current-y) :enter-from :north}}
                    \- {:west  {:current-x (inc current-x) :current-y current-y       :enter-from :west}
                        :east  {:current-x (dec current-x) :current-y current-y       :enter-from :east}}
                    \L {:north {:current-x (inc current-x) :current-y current-y       :enter-from :west  :turn :left}
                        :east  {:current-x current-x       :current-y (dec current-y) :enter-from :south :turn :right}}
                    \J {:north {:current-x (dec current-x) :current-y current-y       :enter-from :east  :turn :right}
                        :west  {:current-x current-x       :current-y (dec current-y) :enter-from :south :turn :left}}
                    \7 {:south {:current-x (dec current-x) :current-y current-y       :enter-from :east  :turn :left}
                        :west  {:current-x current-x       :current-y (inc current-y) :enter-from :north :turn :right}}
                    \F {:south {:current-x (inc current-x) :current-y current-y       :enter-from :west  :turn :right}
                        :east  {:current-x current-x       :current-y (inc current-y) :enter-from :north :turn :left}}
                    nil)))))

(comment

  (def map-data (parse-map example-input-4))
  (def pos (atom nil))

  (reset! pos {:current-x 1 :current-y  2  :enter-from :west})
  (swap! pos (partial follow-segment map-data))




  )

(defn find-starting-options
  "Takse parsed map-input, and returns 2 vectors [x y :direction] from which pipe navigation may start."
  [{[s-x s-y] :S
    [size-x size-y] :size
    :as       map-data}]
  (filter some? [(when (< s-x (dec size-x))
                   (follow-segment map-data {:current-x (inc s-x) :current-y s-y :enter-from :west}))
                 (when (> s-x 0)
                   (follow-segment map-data {:current-x (dec s-x) :current-y s-y :enter-from :east}))
                 (when (< s-y (dec size-y))
                   (follow-segment map-data {:current-x s-x :current-y (inc s-y) :enter-from :north}))
                 (when (> s-y 0)
                   (follow-segment map-data {:current-x s-x :current-y (dec s-y) :enter-from :south}))]))

(comment

  (find-starting-options (parse-map example-input-1))
  (find-starting-options (parse-map example-input-4))


  )

(defn solution-1
  [input]
  (let [map-data (parse-map input)
        [first-position & _ :as _starting-options] (find-starting-options map-data)]
    (/ (loop [steps 2
              position first-position]
         (let [next-step (follow-segment map-data position)]
           (if-not next-step
             steps
             (recur (inc steps)
                    next-step))))
       2)))

(comment

  (solution-1 example-input-1) ;; 4
  (solution-1 example-input-2) ;; 4
  (solution-1 example-input-3) ;; 8
  (solution-1 example-input-4) ;; 8

  (solution-1 (-common/day-input 2023 10))

  )

(defn walk-the-pipes
  [map-data]
  (let [[state & _ :as probably-two-valid-positions-after-two-steps] (find-starting-options map-data)]
    (iteration (fn [{:keys [enter-from] :as step}]
                 (when step
                   (let [next-step (follow-segment map-data enter-from)])
                   ))
               :initk state)))

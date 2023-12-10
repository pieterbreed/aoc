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
  [{:keys [lookup] :as map-data} [current-x current-y enter-from]]
  (let [segment (lookup current-x current-y)]
    (enter-from (case segment
                  \| {:south [current-x       (dec current-y) :south]
                      :north [current-x       (inc current-y) :north]}
                  \- {:west  [(inc current-x) current-y       :west]
                      :east  [(dec current-x) current-y       :east]}
                  \L {:north [(inc current-x) current-y       :west]
                      :east  [current-x       (dec current-y) :south]}
                  \J {:north [(dec current-x) current-y       :east]
                      :west  [current-x       (dec current-y) :south]}
                  \7 {:south [(dec current-x) current-y       :east]
                      :west  [current-x       (inc current-y) :north]}
                  \F {:south [(inc current-x) current-y       :west]
                      :east  [current-x       (inc current-y) :north]}
                  nil))))

(comment

  (def map-data (parse-map example-input-4))
  (def pos (atom nil))

  (reset! pos [1 2 :west])
  (swap! pos (partial follow-segment map-data))

  )

(defn find-starting-options
  "Takse parsed map-input, and returns 2 vectors [x y :direction] from which pipe navigation may start."
  [{[s-x s-y] :S
    [size-x size-y] :size
    :keys     [S lookup]
    :as       map-data}]
  (filter some? [(when (< s-x (dec size-x))
                   (follow-segment map-data [(inc s-x) s-y :west]))
                 (when (> s-x 0)
                   (follow-segment map-data [(dec s-x) s-y :east]))
                 (when (< s-y (dec size-y))
                   (follow-segment map-data [s-x (inc s-y) :north]))
                 (when (> s-y 0)
                   (follow-segment map-data [s-x (dec s-y) :south]))]))

(comment

  (find-starting-options (parse-map example-input-1))
  (find-starting-options (parse-map example-input-4))


  )

(defn solution-1
  [input]
  (let [map-data (parse-map input)
        [dir1 & _] (find-starting-options map-data)]
    (/ (loop [steps 2
              dir   dir1]
         (let [next-step (follow-segment map-data dir)]
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

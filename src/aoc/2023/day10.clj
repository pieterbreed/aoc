(ns aoc.2023.day10
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.core.async :as csp]
            [clojure.set :as set]))


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
                    \| {:south {:current-x current-x       :current-y (dec current-y) :enter-from :south }
                        :north {:current-x current-x       :current-y (inc current-y) :enter-from :north }}
                    \- {:west  {:current-x (inc current-x) :current-y current-y       :enter-from :west }
                        :east  {:current-x (dec current-x) :current-y current-y       :enter-from :east }}
                    \L {:north {:current-x (inc current-x) :current-y current-y       :enter-from :west  :turned :left}
                        :east  {:current-x current-x       :current-y (dec current-y) :enter-from :south :turned :right}}
                    \J {:north {:current-x (dec current-x) :current-y current-y       :enter-from :east  :turned :right}
                        :west  {:current-x current-x       :current-y (dec current-y) :enter-from :south :turned :left}}
                    \7 {:south {:current-x (dec current-x) :current-y current-y       :enter-from :east  :turned :left}
                        :west  {:current-x current-x       :current-y (inc current-y) :enter-from :north :turned :right}}
                    \F {:south {:current-x (inc current-x) :current-y current-y       :enter-from :west  :turned :right}
                        :east  {:current-x current-x       :current-y (inc current-y) :enter-from :north :turned :left}}
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
  (filter some? [(let [starting-pos {:current-x (inc s-x) :current-y s-y :enter-from :west}]
                   (when (and (< s-x (dec size-x))
                              (follow-segment map-data starting-pos))
                     starting-pos))

                 (let [starting-pos {:current-x (dec s-x) :current-y s-y :enter-from :east}]
                   (when (and (> s-x 0)
                              (follow-segment map-data starting-pos))
                     starting-pos))

                 (let [starting-pos {:current-x s-x :current-y (inc s-y) :enter-from :north}]
                   (when (and (< s-y (dec size-y))
                              (follow-segment map-data starting-pos))
                     starting-pos))

                 (let [starting-pos {:current-x s-x :current-y (dec s-y) :enter-from :south}]
                   (when (and (> s-y 0)
                              (follow-segment map-data starting-pos))
                     starting-pos))]))

(comment

  (find-starting-options (parse-map example-input-1))
  (find-starting-options (parse-map example-input-4))


  )

(defn solution-1
  [input]
  (let [map-data (parse-map input)
        [second-position & _ :as _starting-options] (find-starting-options map-data)]
    (/ (loop [steps    1
              position second-position]
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
  (let [[second-position & _ :as _starting-positions] (find-starting-options map-data)
        {[start-x start-y] :S} map-data
        walk (concat [{:current-x start-x :current-y start-y :S? true}
                      second-position]
                     (sequence (iteration (fn [step] (when step (follow-segment map-data step)))
                                          :initk second-position)))
        ;; we attach the :turned indicator one step too late, so we have to shift them backwards by one
        moved-turning-indicators (map (fn [item next-item]
                                        (cond-> (dissoc item :turned)
                                          (:turned next-item)
                                          (assoc :turn (:turned next-item))))
                                      walk
                                      (rest walk))]
    moved-turning-indicators
    ))

(comment

  (find-starting-options (parse-map example-input-1))
  (walk-the-pipes (parse-map example-input-1))

  )

(defn make-drawer:pipesystem
  [map-data pipeline-traversal]
  (let [segments (->> pipeline-traversal
                      (map (juxt (juxt :current-x :current-y)
                                 (fn [{:keys [current-x current-y] :as step}]
                                   ((:lookup map-data) current-x current-y))))
                      (into {}))]
    (fn [x y] (get segments [x y]))))

(defn draw-with-drawers
  [map-data & drawer-fns]
  (let [draw-first (fn [x y] (->> (concat drawer-fns [(constantly \.)])
                                  (keep #(% x y))
                                  (first)))]
    (->> (for [y (range (-> map-data :size second))
               x (range (-> map-data :size first))]
           (draw-first x y))
         (partition (-> map-data :size first))
         (map (partial apply str))
         (str/join "\n"))))

(defn draw-pipesystem!
  [map-data traversal]
  (let [drawer (make-drawer:pipesystem map-data traversal)]
    (->> (for [y (range (-> map-data :size second))
               x (range (-> map-data :size first))]
           (drawer x y))
         (partition (-> map-data :size first))
         (map (partial apply str))
         (str/join "\n"))))

(comment

  (println (let [map-data (parse-map example-input-4)
                 traversal (walk-the-pipes map-data)
                 drawer (make-drawer:pipesystem map-data traversal)
                 drawing (->> (for [y (range (-> map-data :size second))
                                    x (range (-> map-data :size first))]
                                (drawer x y))
                              (partition (-> map-data :size first))
                              (map (partial apply str))
                              (str/join "\n"))]
             drawing))



  )

(defn traversal-handedness
  [traversal]
  (as-> traversal $
    (keep :turn $)
    (frequencies $)
    (sort-by second $)
    (last $)
    (first $)))

(comment

  (traversal-handedness (walk-the-pipes (parse-map example-input-1))) ;; :right
  (traversal-handedness (walk-the-pipes (parse-map example-input-3))) ;; :right

  )

(defn find-possible-inside-blocks
  "This will walk the traversal and mark all blocks that are on the inside of any section.
  These marked blocks might still be pipe sections part of the main loop. This just finds
  the set of possibly-ground and immediate inside next to the pipe sections being traversed."
  [traversal]
  (let [handedness (traversal-handedness traversal)]
    (set (concat (->> traversal
                      (filter (complement :S?))
                      (filter (complement :turn))
                      ;; we find all the straight edges, and mark the one block on the inside of the straight edge
                      (map (fn [{:keys [enter-from
                                        current-x
                                        current-y]}]
                             (case [enter-from handedness]
                               [:west :right]  [current-x       (inc current-y)]
                               [:west :left]   [current-x       (dec current-y)]
                               [:east :right]  [current-x       (dec current-y)]
                               [:east :left]   [current-x       (inc current-y)]
                               [:north :right] [(dec current-x) current-y]
                               [:north :left]  [(inc current-x) current-y]
                               [:south :right] [(inc current-x) current-y]
                               [:south :left]  [(dec current-x) current-y]))))
                 (->> traversal
                      (filter (complement :S?))
                      (filter :turn)
                      ;; now we're looking at the turns, and marking inside blocks around the outside of corners
                      (mapcat (fn [{:keys [enter-from turn
                                           current-x current-y]}]
                                (case [enter-from handedness turn ]
                                  [:south :right :left] [[(inc current-x) current-y]
                                                         [(inc current-x) (dec current-y)]
                                                         [current-x       (dec current-y)]]
                                  [:south :left :right] [[(dec current-x) current-y]
                                                         [(dec current-x) (dec current-y)]
                                                         [current-x       (dec current-y)]]

                                  [:west :right :left] [[current-x       (inc current-y)]
                                                        [(inc current-x) (inc current-y)]
                                                        [(inc current-x) current-y]]
                                  [:west :left :right] [[current-x       (dec current-y)]
                                                        [(inc current-x) (dec current-y)]
                                                        [(inc current-x) current-y]]

                                  [:north :right :left] [[(dec current-x) current-y]
                                                         [(dec current-x) (inc current-y)]
                                                         [current-x       (inc current-y)]]
                                  [:north :left :right] [[(inc current-x) current-y]
                                                         [(inc current-x) (inc current-y)]
                                                         [current-x       (inc current-y)]]

                                  [:east :right :left] [[(dec current-x) current-y]
                                                        [(dec current-x) (dec current-y)]
                                                        [current-x       (dec current-y)]]
                                  [:east :left :right] [[(dec current-x) current-y]
                                                        [(dec current-x) (inc current-y)]
                                                        [current-x       (inc current-y)]]

                                  []))))))))


(comment

  (let [map-data (parse-map example-input-1)
        traversal (walk-the-pipes map-data)
        pipe-segment-drawer (make-drawer:pipesystem map-data traversal)
        possible-inside? (find-possible-inside-blocks traversal)
        possible-inside-drawer (fn [x y]
                                 (when (possible-inside? [x y]) \I))]
    (println (draw-with-drawers map-data pipe-segment-drawer possible-inside-drawer))
    possible-inside?)

  )

(defn investigate-possible-insides
  [{:keys [map-data
           traversal
           pipe-segment-drawer]
    :as    context}
   {:keys [possible-insides
           confirmed-inside
           confirmed-not-inside]
    :as   state}]
  (let [all-known (set/union confirmed-inside
                             confirmed-not-inside)
        {insides false
         outsides true} (into {}
                              (comp (filter (complement all-known))
                                    (map (juxt identity #(some? (apply pipe-segment-drawer %))))
                                    (x/by-key second (x/into [])))
                              possible-insides)
        new-possible-insides (->> insides
                                  (map first)
                                  (mapcat (fn [[x y]]
                                            [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
                                             [(dec x) y]                   [(inc x) y]
                                             [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]))
                                  (filter (complement all-known))
                                  (set))
        next-result
          (-> state
              (assoc :possible-insides new-possible-insides)
              (update :confirmed-inside set/union (set (map first insides)))
              (update :confirmed-not-inside set/union (set (map first outsides))))]

    (if (= next-result state)
      next-result
      (recur context next-result))))


(defn solution-2
  "Eish!
  - need to find out the handed-ness (of the traversal) of the pipe system
  - based on handedness (of the traversal) claim all of the inside blocks
  - for each claimed block, start a recursive descent space-filler to claim all of the blocks inside the curve."
  [input]
  (let [map-data (parse-map input)
        pipeline-traversal (walk-the-pipes map-data)
        pipe-segment-drawer (make-drawer:pipesystem map-data pipeline-traversal)
        handedness (traversal-handedness pipeline-traversal)
        initial-positions-to-be-investigated (find-possible-inside-blocks pipeline-traversal)
        investigations-result (investigate-possible-insides {:map-data map-data
                                                             :traversal pipeline-traversal
                                                             :pipe-segment-drawer pipe-segment-drawer}
                                                            {:possible-insides initial-positions-to-be-investigated
                                                             :confirmed-inside #{}
                                                             :confirmed-not-inside #{}})]
    (count (:confirmed-inside investigations-result))

    ))

(def example-input-5
  "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def example-input-6
  "..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........")

(def example-input-7
  ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def example-input-8
  "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(comment

  (solution-2 example-input-1) ;; 1
  (solution-2 example-input-4) ;; 1
  (solution-2 example-input-5) ;; 4
  (solution-2 example-input-6) ;; 4
  (solution-2 example-input-7) ;; 8
  (solution-2 example-input-8) ;; 10

  (solution-2 (-common/day-input 2023 10))



  )

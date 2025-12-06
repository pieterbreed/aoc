(ns aoc.2025.day4
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   ))

(defn parse-input
  [input]
  (let [input-lines (str/split-lines input)
        h (count input-lines)
        w (count (first input-lines))]
    {:height h
     :width w
     :data   input-lines}))

(def test-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(comment

  (parse-input test-input)

  )

(defn has-roll?
  "Returns boolean of position at x y contains a roll \\@"
  [{:as puzzle-state
    :keys [height width data]}
   x y]
  (cond
    (< x 0)       false
    (>= x width)  false
    (< y 0)       false
    (>= y height) false
    :else
    (= \@ (get-in data [y x]))))

(comment

  (def x (parse-input test-input))
  (has-roll? x 0 0) ;;
  (has-roll? x - 0)

  )

(defn roll-outside?
  "Returns true if a roll at a position (if it is a roll...) is surrounded by less than 4 other rolls."
  [puzzle-state x y]
  (boolean
   (when (has-roll? puzzle-state x y)
     (let [tl (has-roll? puzzle-state (dec x) (dec y))
           tm (has-roll? puzzle-state x       (dec y))
           tr (has-roll? puzzle-state (inc x) (dec y))
           ml (has-roll? puzzle-state (dec x) y)
           mr (has-roll? puzzle-state (inc x) y)
           bl (has-roll? puzzle-state (dec x) (inc y))
           bm (has-roll? puzzle-state x       (inc y))
           br (has-roll? puzzle-state (inc x) (inc y))]
       (< (->> [tl tm tr
                ml mr
                bl bm br]
               (filter true?)
               (count))
          4)))))

(comment

  (roll-outside? x  2 0)
  (roll-outside? x  3 0)
  (roll-outside? x  1 0)

  )

(defn solve-1
  [input]
  (let [puzzle-input (parse-input input)]
    (->> (for [x (range (:width puzzle-input))
               y (range (:height puzzle-input))]
           (roll-outside? puzzle-input x y))
         (filter true?)
         (count))))

(comment

  (solve-1 test-input) ;; 13
  (solve-1 (-common/day-input 2025 4)) ;;

  )

(defn parse-input-2
  [input]
  (let [input-lines (str/split-lines input)
        h (count input-lines)
        w (count (first input-lines))]
    {:height h
     :width w
     :data  (mapv vec input-lines)}))

(comment

  (def xxx (parse-input-2 test-input))
  (has-roll? xxx 2 0)
  (roll-outside? xxx 2 0)

  )

(defn solve-2
  [input]
  (let [puzzle-input (parse-input-2 input)]
    (loop [puzzle-input puzzle-input
           removable    0]
      (let [removable-rolls
            (vec (for [x (range (:width puzzle-input))
                       y (range (:height puzzle-input))
                       :when (roll-outside? puzzle-input x y)]
                   [x y]))

            how-many (count removable-rolls)]
        (if (zero? how-many)
          removable
          (recur (reduce (fn [puzzle-state [x y :as _removable]]
                           (assoc-in puzzle-state [:data y x] \.))
                         puzzle-input
                         removable-rolls)
                 (+ removable how-many)))))))

(comment

  (solve-2 test-input) ;; 43
  (solve-2 (-common/day-input 2025 4)) ;;


  )

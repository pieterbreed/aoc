(ns aoc.2025.day1
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))

(def total-positions 100)

(defn rotate
  [position instruction-s]
  (let [d      (case (get instruction-s 0)
                 \L :left
                 \R :right)
        points (Long/parseLong (subs instruction-s 1))
        points' (if (= :right d)
                  points
                  (* -1 points))]

    (mod (+ position points')
         total-positions)))

(defn perform-rotations-1
  [input start]
  (let [lines (str/split-lines input)]
    (loop [pos            start
           zero-pos-count 0
           [hd & rst]    lines]
      (if (nil? hd)
        zero-pos-count
        (let [next-position (rotate pos hd)]
          (recur next-position
                 (if (zero? next-position) (inc zero-pos-count) zero-pos-count)
                 rst))))))

(comment

  (def test-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")
  (perform-rotations test-input 50) ;; 3
  (perform-rotations (-common/day-input 2025 1) 50 ) ;;

  )

(defn decode-rotation
  [instruction-s]
  (let [d      (case (get instruction-s 0)
                 \L :left
                 \R :right)
        points (Long/parseLong (subs instruction-s 1))
        points' (if (= :right d)
                  points
                  (* -1 points))]

    points'))


(defn perform-rotations-2
  [input start]
  (let [lines (str/split-lines input)]
    (loop [pos            start
           zero-pos-count 0
           [hd & rst]    lines]
      (if (nil? hd)
        zero-pos-count
        (let [rotation-points  (decode-rotation hd)
              extra-0s         (abs (quot rotation-points
                                          total-positions))
              effective'        (mod rotation-points
                                     total-positions)
              effective       (if (and (neg? rotation-points)
                                       (not (zero? effective')))
                                (- effective' total-positions)
                                effective')
              cross-once-more? (cond
                                 (and (not (zero? pos))
                                      (neg? effective)
                                      (>= (abs effective)
                                          pos))
                                 1

                                 (and (not (zero? pos))
                                      (pos? effective)
                                      (>= effective
                                          (- total-positions
                                             pos)))
                                 1

                                 :else
                                 0)
              next-pos (mod (+ pos effective) total-positions)]
          (recur next-pos
                 (+ zero-pos-count
                    extra-0s
                    cross-once-more?)
                 rst))))))

(comment

  (perform-rotations-2 test-input 50) ;; 6
  (perform-rotations-2 (-common/day-input 2025 1) 50) ;;  2406

  (def test-input-2 "L68
L300
")
  (perform-rotations-2 test-input-2 50)

  )

(ns aoc.2025.day9
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   [net.cgrand.xforms :as x]
   [clojure.java.io :as io]
   ))

(def test-input
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map #(str/split % #","))
       (mapv #(mapv Long/parseLong %))))

(comment

  (parse-input test-input)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn area'
  ([points-set]
   (let [[[x1 y1 :as _p1]
          [x2 y2 :as _p2]] (vec points-set)]
     (area [x1 y1] [x2 y2])))
  ([[x1 y1] [x2 y2]]
   (* (inc (abs (- x1 x2)))
      (inc (abs (- y1 y2))))))

(def area (memoize area')) ;; NOTE: This is totally unnecessary...

(comment

  (area #{[2 5] [9 7]}) ;; 24
  (area #{[7 1] [11 7]}) ;; 35
  (area #{[7 3] [2 3]}) ;; 6
  (area #{[2 5] [11 1]}) ;; 50

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn all-pairs-and-areas
  [input]
  (let [s (count input)
        all-pairs
        (->> (for [i (range s)
                   j (range (inc i) s)]
               [i (get input i)
                j (get input j)
                (area  #{(get input i)
                         (get input j)})])
             (into #{}))]
    all-pairs))

(comment

  (all-pairs-and-areas (parse-input test-input))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solve-1
  [input]
  (let [input (parse-input input)
        pairs (all-pairs-and-areas input)]
    (->> pairs
         (map last)
         (reduce max))))

(comment

  (solve-1 test-input) ;; 50
  (solve-1 (-common/day-input 2025 9)) ;;

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn points-on-line
  [[x1 y1 :as _p1] [x2 y2 :as _p2]]
  (cond
    (and (= x1 x2)
         (< y1 y2))
    (for [y (range y1 (inc y2))] [x1 y])

    (and (= x1 x2)
         (> y1 y2))
    (for [y (range y2 (inc y1))] [x1 y])

    (and (= y1 y2)
         (< x1 x2))
    (for [x (range x1 (inc x2))] [x y1])

    (and (= y1 y2)
         (> x1 x2))
    (for [x (range x2 (inc x1))] [x y1])))

(comment

  (points-on-line [7 1] [11 1])  ;; ([7 1]  [8 1]  [9 1]  [10 1] [11 1])
  (points-on-line [11 1] [11 7]) ;; ([11 1] [11 2] [11 3] [11 4] [11 5] [11 6] [11 7])
  (points-on-line [11 7] [9 7])  ;; ([9 7]  [10 7] [11 7])
  (points-on-line [9 7] [9 5])   ;; ([9 5]  [9 6]  [9 7])

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *blank-canvas* {:red  #{} :green #{}})

(defn draw-outline-to-canvas
  "Draws the input (outline) onto an area (return value)"
  [canvas input]
  (let [input' (conj input (first input)) ;; add the first point at the end, to complete the last line
        ]
    (loop [cp             (first input')
           [nxt & rst]    (rest input')
           canvas'        canvas]
      (if-not nxt canvas'
              (recur nxt
                     rst
                     (-> canvas'
                         (update :green into (points-on-line cp nxt))
                         (update :red   into [cp])))))))

;; the thing racing has taught me is that, when you are fast, the fast lives in your head
;; when you are slow, your mind also is slow

(defn canvas->text-file
  [{:as canvas :keys [red green]}
   file-name]
  (let [points (set/union red green)
        min-x  (->> points (map first)  (reduce min) (dec))
        max-x  (->> points (map first)  (reduce max) (inc))
        min-y  (->> points (map second) (reduce min) (dec))
        max-y  (->> points (map second) (reduce max) (inc))]
    (with-open [s (io/output-stream (java.io.File. file-name))]
      (doseq [y (range min-y
                       (inc max-y))]
        (let [sb (StringBuilder.)]
          (doseq [x (range min-x
                           (inc max-x))]
            (.append sb (str (cond
                               ((:red canvas)   [x y]) "🟥"
                               ((:green canvas) [x y]) "🟩"
                               :else                   "⬛"))))

          (.write s (.getBytes (str (.toString sb) "\n"))))))))

(comment

  (canvas->text-file (draw-outline-to-canvas *blank-canvas* (parse-input test-input))
                     "test.txt")

  (draw-canvas (draw-outline-to-canvas test-input))
  (draw-canvas (draw-outline-to-canvas (-common/day-input 2025 9)))

  (println (draw-canvas (draw-outline-to-canvas (-common/day-input 2025 9))))


  )

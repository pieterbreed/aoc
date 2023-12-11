(ns aoc.2023.day11
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.core.async :as csp]
            [clojure.set :as set]))

(def example-input-1
  "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn analyze-map
  "Takes a string map and analyze it.

  Returns the size [x y] and :get-pixel a (fn [x y]) that can fetch the char at the coord."
  [s]
  (let [rows (str/split-lines s)]
    {:size [(count (first rows))
            (count rows)]
     :rows rows
     :get-pixel
     (fn [x y]
       (get-in rows [y x]))}))

(defn print-map
  [{:keys [get-pixel]
    [size-x size-y] :size
    :as map-data}]
  (->> (for [y (range size-y)
             x (range size-x)]
         (get-pixel x y))
       (partition size-x)
       (map #(apply str %))
       (str/join "\n"))
  )

(defn find-empty-rows
  [{[size-x size-y] :size
    :keys [get-pixel
           rows]
    :as map-data}]
  (let [empty-row? (fn [i] (->> (get rows i)
                                (every? #(= \. %))))]
    (for [y (range size-y)
          :when (empty-row? y)]
      y)))

(defn find-empty-columns
  [{[size-x size-y] :size
    :keys [get-pixel
           rows]
    :as map-data}]
  (for [x (range size-x)
             :let [items (for [y (range size-y)]
                           (get-pixel x y))]
             :when (every? #(= \. %) items)]
    x))

(defn expand-rows
  [{:keys [rows]
    :as map-data}
   factor]
  (let [empty-row? (set (find-empty-rows map-data))
        rows (->> rows
                  (map-indexed (fn [i r] [i r]))
                  (mapcat (fn [[i r]]
                            (if (empty-row? i)
                              (repeat factor r)
                              [r])))
                  (vec))]
    {:rows rows
     :get-pixel (fn [x y] (get-in rows [y x]))
     :size [(count (first rows))
            (count rows)]}))

(defn expand-columns
  [{:keys [rows]
    :as map-data}
   factor]
  (let [empty-col? (set (find-empty-columns map-data))
        rows (mapv (fn [row]
                     (->> row
                          (map-indexed (fn [col ch] [col ch]))
                          (mapcat (fn [[col x]]
                                    (if (empty-col? col)
                                      (repeat factor x)
                                      [x])))
                          (apply str)))
                   rows)]
    {:rows rows
     :get-pixel (fn [x y] (get-in rows [y x]))
     :size [(count (first rows))
            (count rows)]}))

(defn find-galaxies-with-coords
  [{[size-x size-y] :size
    :keys [get-pixel
           rows]
    :as map-data}]

  (let [galaxies (for [x (range size-x)
                       y (range size-y)
                       :let [ch (get-pixel x y)]
                       :when (= \# ch)]
                   [x y])]
    galaxies))

(defn parse-starmap-1
  "Reads input, apply cosmic expansion.
  Return coordinates for every galaxy in expanded universe."
  [input & {:keys [expansion-factor]
            :or   {expansion-factor 2}}]
  (let [initial-map (analyze-map input)
        expanded-map (-> initial-map
                         (expand-rows expansion-factor)
                         (expand-columns expansion-factor))]
    expanded-map))

(comment

  (let [sm (parse-starmap-1 example-input-1 10)]
    (println (print-map sm)))

  )

(defn find-galaxy-pairs
  [galaxies-coords]
  (loop [[g1 & g-rest] galaxies-coords
         acc []]
    (if (not (and g1 g-rest))
      acc
      (recur g-rest
             (concat acc
                     (for [g g-rest]
                       [g1 g]))))))

(defn shortest-distance-in-steps
  "Shortest grid-distance, sum of differences in x and y."
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn solution-1
  [input]
  (let [map-data (parse-starmap-1 input)
        galaxies (find-galaxies-with-coords map-data)
        all-pairs (find-galaxy-pairs galaxies)]
    (->> all-pairs
         (map #(apply shortest-distance-in-steps %))
         (reduce +))))

(comment

  (solution-1 example-input-1) ;; 374
  (solution-1 (-common/day-input 2023 11))


  )


(defn solution-2
  [input factor]
  (let [map-data (parse-starmap-1 input :expansion-factor factor)
        galaxies (find-galaxies-with-coords map-data)
        all-pairs (find-galaxy-pairs galaxies)]
    (->> all-pairs
         (map #(apply shortest-distance-in-steps %))
         (reduce +))))

(comment

  (solution-2 example-input-1 10)

  )

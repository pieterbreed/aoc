(ns aoc.2025.day7
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   [net.cgrand.xforms :as x]
   ))

(defn find-all-indices-of
  "Returns all indices of `\\ch` in `s`."
  [s ch]
  (->> s
       (map-indexed vector)
       (keep (fn [[i ch']] (when (= ch ch') i)))
       (into #{})))

(defn solve-1
  [s]
  (let [[source-line & splitter-lines] (str/split-lines s)
        beams (find-all-indices-of source-line \S)
        splitters (->> splitter-lines
                       (filter #(str/includes? % "^"))
                       (map (fn [splitter-line]
                              (let [is (find-all-indices-of splitter-line \^)]
                                (fn [[beams splits]]
                                  (let [new-splits (set/intersection beams is)
                                        new-beams (into #{}
                                                        (mapcat (fn [i]
                                                                  #{(inc i) (dec i)}))
                                                        new-splits)]
                                    [(-> beams
                                         (set/difference new-splits)
                                         (set/union new-beams))
                                     (+ splits (count new-splits))])))))
                       (reverse)
                       (apply comp))]
    (splitters [beams 0])))

(def test-input'
  ".......S.......
...............
.......^.......
...............
......^.^......")

(def test-input
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(comment

  (solve-1 test-input) ;; [#{0 4 6 12 2 11 14 10 8} 21] ;; => 21
  (second (solve-1 (-common/day-input 2025 7))) ;;

  )

(defn solve-2
  [s]
  (let [[source-line & splitter-lines] (str/split-lines s)
        beams (find-all-indices-of source-line \S)
        splitters (->> splitter-lines
                       (filter #(str/includes? % "^"))
                       (map (fn [splitter-line]
                              (let [splitter-indices (find-all-indices-of splitter-line \^)]
                                (fn [[beams nr-universes]]
                                  (let [{splits     true
                                         non-splits false} (group-by (comp boolean splitter-indices first)
                                                                     beams)
                                        new-beams (into {}
                                                        (comp (mapcat (fn [[beam-index beam-count]]
                                                                        [[(inc beam-index) beam-count]
                                                                         [(dec beam-index) beam-count]]))
                                                              (x/by-key first
                                                                        (comp (map second)
                                                                              (x/reduce +))))
                                                        splits)

                                        _ (tap> [:new-beams new-beams
                                                 :splits splits
                                                 :non-splits non-splits])]
                                    [
                                     (merge-with +
                                                 (into {} non-splits)
                                                 new-beams)
                                     (+ nr-universes (->> splits
                                                          (map second)
                                                          (reduce +)))
                                     ])))))
                       (reverse)
                       (apply comp))
        r (splitters [(->> beams
                             (map (fn [b] [b 1]))
                             (into {}))
                        1])]
    (tap> r)
    (second r)))



(comment

  (solve-2 test-input') ;; 4
  (solve-2 test-input) ;; 40
  (solve-2 (-common/day-input 2025 7)) ;;

  )

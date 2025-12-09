(ns aoc.2025.day8
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   [net.cgrand.xforms :as x]
   ))

(def test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

;; by using a set for the parameters, the order of the two points don't matter for memoization
(defn p-distance
  [points-set]
  (let [[[x1 y1 z1 :as _p1]
         [x2 y2 z2 :as _p2]] (vec points-set)]
    (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                  (* (- y1 y2) (- y1 y2))
                  (* (- z1 z2) (- z1 z2))))))

(def distance (memoize p-distance))

(comment

  (p-distance #{[162 817 812]
                [425 690 689]}) ;; 316.90219311326956
  (distance #{[162 817 812]
              [425 690 689]})
  (distance #{[425 690 689]
              [162 817 812]})


  )

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #","))
       (mapv (fn [coords] (mapv Long/parseLong coords) ))))

(comment

  (parse-input test-input)

  )

(defn closest-pair-and-rest
  [all-pairs]
  (let [closest-pair
        (reduce (fn [acc x]
                  (if (< (last x) (last acc))
                    x
                    acc))
                all-pairs)]
    [closest-pair (disj all-pairs closest-pair)]))

(defn solve-1-naive
  [input max-steps]
  (let [input (parse-input input)
        s     (count input)

        all-pairs
        (->> (for [i (range s)
                   j (range (inc i) s)]
               [i (get input i)
                j (get input j)
                (distance #{(get input i)
                            (get input j)})])
             (into #{}))

        {:as   after-steps
         :keys [circuits]}
        (loop [state {:remaining-pairs all-pairs
                      :circuits        #{}}
               steps max-steps]
          (if (zero? steps) state
              (let [[[_p1i p1 _p2i p2 _pair-dist :as _closest] remaining-pairs]
                    (closest-pair-and-rest (:remaining-pairs state))

                    in-circuits? (->> (:circuits state)
                                      (filter (fn [c] (seq (set/intersection c #{p1 p2})))))

                    remaining-circuits (if (seq in-circuits?)
                                         (set/difference (:circuits state) (set in-circuits?))
                                         (:circuits state))

                    new-circuit (into (or (when (seq in-circuits?)
                                            (reduce set/union #{} in-circuits?))
                                          #{})
                                      [p1 p2])]

                (recur {:remaining-pairs remaining-pairs
                        :circuits        (conj remaining-circuits new-circuit)}
                       (dec steps)))))]

    (->> circuits
         (sort-by count)
         (reverse)
         (take 3)
         (map count)
         (reduce *))))

(comment

  (solve-1-naive test-input 10) ;; 40
  (solve-1-naive (-common/day-input 2025 8) 1000) ;; 29406

  )

(defn solve-2
  [input]
  (let [input (parse-input input)
        s     (count input)

        all-pairs
        (->> (for [i (range s)
                   j (range (inc i) s)]
               [i (get input i)
                j (get input j)
                (distance #{(get input i)
                            (get input j)})])
             (into #{}))

        [[x1 & _] [x2 & _] :as _final-pair]
        (loop [state {:remaining-pairs all-pairs
                      :circuits        #{}}]
          (let [[[_p1i p1 _p2i p2 _pair-dist :as _closest] remaining-pairs]
                (closest-pair-and-rest (:remaining-pairs state))

                in-circuits? (->> (:circuits state)
                                  (filter (fn [c] (seq (set/intersection c #{p1 p2})))))

                remaining-circuits (if (seq in-circuits?)
                                     (set/difference (:circuits state) (set in-circuits?))
                                     (:circuits state))

                new-circuit (into (or (when (seq in-circuits?)
                                        (reduce set/union #{} in-circuits?))
                                      #{})
                                  [p1 p2])

                next-circuits (conj remaining-circuits new-circuit)

                next-state {:remaining-pairs remaining-pairs
                            :circuits        next-circuits}]

            (if (and (= 1 (count next-circuits))
                     (= (set input) (first next-circuits)))
              [p1 p2]
              (recur next-state))))]

    (* x1 x2)))

(comment

  (solve-2 test-input) ;; 25272
  (solve-2 (-common/day-input 2025 8)) ;;



  )

(ns aoc.2023.day5
  (:require [aoc.common :as -common]
            [instaparse.core :as ip]
            [clojure.core.async :as csp]))

(def example-input
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def almanac-parser
  (ip/parser
   "almanac = seeds-to-plant <whitespace> (map <whitespace>?)+
    seeds-to-plant = <'seeds:'> (<whitespace> number)+
    map = from <'-to-'> to <' map:\n'> (range <'\n'>?)+
    range = number <whitespace> number <whitespace>  number
    from = elements
    to = elements
    <elements> = 'seed' | 'soil' | 'fertilizer' | 'water' | 'light' | 'temperature' | 'humidity' | 'location'
    number = #'[0-9]+'
    <whitespace> = #'\\s+' "))

(defn parse-almanac
  [almanac-str]
  (->> almanac-str
       (ip/parse almanac-parser)
       (ip/transform (or {:number #(Long/parseLong %)

                          :range   (fn [dest-start src-start nr]
                                   (fn [src]
                                     (when (< (dec src-start)
                                              src
                                              (+ src-start nr))
                                       (+ dest-start
                                          (- src src-start)))))
                          :map     (fn [[_ from] [_ to] & ranges]
                                     (let [lookup (fn [src]
                                                    (or (->> ranges
                                                             (keep #(%1 src))
                                                             (first))
                                                        src))]
                                   [[from to] lookup]))
                          :almanac (fn [[_ & seeds] & ranges]
                                     {:seeds  seeds
                                      :ranges (into {} ranges)})}))))

(comment

  (parse-almanac example-input)
  (parse-almanac (-common/day-input 2023 5))

  )

(defn lookup
  [almanac-data from to item-nr]
  (or (when-let [lookup (get-in almanac-data [:ranges [from to]])]
        (lookup item-nr))
      item-nr))

(defn make-seed->location
  [almanac-data]
  (reduce comp
          (reverse [(partial lookup almanac-data "seed" "soil")
                    (partial lookup almanac-data "soil" "fertilizer")
                    (partial lookup almanac-data "fertilizer" "water")
                    (partial lookup almanac-data "water" "light")
                    (partial lookup almanac-data "light" "temperature")
                    (partial lookup almanac-data "temperature" "humidity")
                    (partial lookup almanac-data "humidity" "location")])))

(comment

  (let [a (parse-almanac example-input)]
    (for [seed (:seeds a)]
      [seed ((make-seed->location a) seed)]))

  )

(defn solution-1
  [almanac-str]
  (let [{:keys [seeds]
         :as   almanac-data}
        (parse-almanac almanac-str)
        seed->location (make-seed->location almanac-data)]
    (->> seeds
         (map seed->location)
         (apply min))))

(comment

  (parse-almanac example-input)

  (solution-1 example-input) ;; 35
  (solution-1 (-common/day-input 2023 5))

  )

(defn solution-2
  [almanac-str]
  (let [almanac-data (parse-almanac almanac-str)
        seed->location (make-seed->location almanac-data)]
    (apply min (let [{:keys [seeds]} almanac-data
                     seed-ranges     (partition 2 seeds)]
                 (for [[range-start range-length] seed-ranges
                       seed (range range-start (+ range-start range-length))]
                   (seed->location seed))))))

(defn solution-2'
  "Hopefully no worse than `solution-2`, and maybe a little faster."
  [almanac-str]
  (let [almanac-data (parse-almanac almanac-str)

        seed-ranges (partition 2 (:seeds almanac-data))
        seed->location (make-seed->location almanac-data)]
    (loop [[seed-range & seed-range-rest]  seed-ranges
           current-min-location            Long/MAX_VALUE]
      (let [[range-start range-length] seed-range]
        (if-not seed-range
          (do (println "Done.")
              current-min-location)
          (do (println (str "Starting with seed range: "
                            range-start
                            " to "
                            (+ range-start range-length)
                            ". Remaining: "
                            (count seed-range-rest)))
              (recur seed-range-rest
                     (loop [seed                  range-start
                            seeds-remaining       range-length
                            current-min-location' current-min-location]
                       (when (zero? (mod seeds-remaining 1111111))
                         (println (str "Seeds remaining: " seeds-remaining)))
                       (if (<= seeds-remaining 0)
                         current-min-location'
                         (let [this-location (seed->location seed)]
                           (recur (inc seed)
                                  (dec seeds-remaining)
                                  (min current-min-location'
                                       this-location))))))))))))

(defn solution-2-3
  [almanac-str]
  (let [almanac-data (parse-almanac almanac-str)
        seed->location (make-seed->location almanac-data)
        seed-ranges     (partition 2 (:seeds almanac-data))]
    (->> seed-ranges
         (map (fn [[range-start range-length]]
                (csp/thread (let [this-range-min
                                  (->> (range range-start (+ range-start range-length))
                                       (map seed->location)
                                       (reduce min))]
                              (println (str "range: " range-start " to " (+ range-start range-length) ", local minimum: " this-range-min))
                              this-range-min))))
         (doall)
         (map (fn [ch] (csp/<!! ch)))
         (reduce min))))

(comment

  (solution-2 example-input) ;; 46
  (solution-2' example-input)
  (solution-2-3 example-input)

  ;; this one doesn't stop...
  (solution-2 (-common/day-input 2023 5))

  ;; neither does this one
  (solution-2' (-common/day-input 2023 5))

  ;; turns out there's trillions of calculations to do
  ;; this one is now <NR-OF-CPU-CORES> as slow as the previous ones
  ;; which means it finished is more than an hour.
  (solution-2-3 (-common/day-input 2023 5))


  )

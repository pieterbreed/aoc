(ns aoc.2025.day6
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   ))

(def test-input
  "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")


(defn parse-input
  [s]
  (let [ll (str/split-lines s)
        rows
        (into []
              (comp (map str/trim)
                    (map #(str/split % #"\s+"))
                    (map (fn [xs] (filterv (comp pos? count) xs)))
                    (map (fn [xs]
                           (try (mapv Long/parseLong xs)

                              (catch Throwable t
                                  (println xs))))))
              (butlast  ll))

        col-count (count (first rows))

        ops (-> (last ll)
                (str/trim)
                (str/split #"\s+")
                (into []))]

    (when-not (every? (comp #(= % col-count) count)
                      rows)
      (throw (ex-info "rows not the same width" {})))

    {:rows       rows
     :operations ops}))

(comment

  (parse-input test-input)


  )

(defn solve-1
  [input]
  (let [{:keys [rows operations]} (parse-input input)]
    (->> operations
         (map-indexed (fn [i op]
                        (let [opf (case op
                                    "+" +
                                    "*" *)]
                          (->> rows
                               (map #(get % i))
                               (reduce opf)))))
         (reduce +))))

(comment

  (solve-1 test-input) ;; 4277556
  (solve-1 (-common/day-input 2025 6)) ;;

  )

(defn parse-2
  [input]
  (let [all-rows (str/split-lines input)
        rows-with-nrs (butlast all-rows)
        split-columns
        (-> (into [0]
                  (for [c (range (count (first all-rows)))
                        :when (every? (fn [x] (= \space x))
                                      (->> rows-with-nrs
                                           (map #(get % c))))]
                    c))
            (conj :end))

        split-row (fn [row]
                    (->> (map vector split-columns (rest split-columns))
                         (mapv (fn [[l u]]
                                 (if (not= :end u)
                                   (subs row l u)
                                   (subs row l))))))

        seperated-rows-with-nrs
        (mapv split-row rows-with-nrs)

        #_#_groups-of-nrs
        (for [group-i (range (count (first seperated-rows-with-nrs)))]
          [group-i (for [nr-i (range (count (get-in seperated-rows-with-nrs [0 group-i])))]
                     (get-in seperated-rows-with-nrs [``j]))])

        ]
    seperated-rows-with-nrs))

(comment

  (parse-2 test-input)
  (parse-2 (-common/day-input 2025 6))

  )

(defn solve-2-2
  [input]
  (let [grid (str/split-lines input)
        rows (dec (count grid))
        cols (count (first grid))

        operators (-> (last grid)
                      (str/split #"\s+"))

        raw-column-numbers
        (for [col (range cols)]
          (reduce str
                  (for [row (range rows)]
                    (get-in grid [row col]))))

        raw-column-split
        (->> raw-column-numbers
             (partition-by (fn [col] (every? #(= \space %) col)))
             (filter (fn [xs]
                       (< 1 (count xs)))))]

    (reduce +
            (map (fn [nrs op]
                   (let [op (case op "+" + "*" *)]
                     (reduce op
                             (->> nrs
                                  (map str/trim)
                                  (map Long/parseLong)))))
                 raw-column-split
                 operators))))

(comment

  (solve-2-2 test-input) ;; 3263827
  (solve-2-2 (-common/day-input 2025 6)) ;;


  )

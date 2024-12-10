(ns aoc.2024.day4
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))

(def input-1 "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")


(defn parse-input
  "Constructs a structure of \\chars, representing the input."
  ([input]
   (let [;; read-char (comp #{\X \M \A \S})
         ;; read-line #(map read-char %)

         [first-line & rest-lines] (str/split-lines input)
         width                     (count first-line)
         nr-lines                  (inc (count rest-lines))]
     (reduce (fn [state line]
               (update state :layout
                       conj
                       (vec line)))

             {:width    width
              :nr-lines nr-lines
              :layout   [(vec first-line)]}

             rest-lines))))

(defn rows
  [{:as   parsed-input
    :keys [width
           nr-lines
           layout]}]
  (vec (->> (for [line layout]
              (apply str line)))))

(defn rows-in-reverse
  [parsed-input]
  (mapv (comp #(apply str %) reverse) (rows parsed-input)))

(defn columns
  [{:as   parsed-input
    :keys [width
           nr-lines
           layout]}]
  (vec (for [column-index (range width)]
         (apply str (for [row-index (range nr-lines)]
                      (get-in layout [row-index column-index]))))))

(defn columns-in-reverse
  [parsed-input]
  (mapv (comp #(apply str %) reverse) (columns parsed-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pick-diagonal
  "Goes down and left until out-of-bounds"
  [{:as   parsed-input
    :keys [nr-lines
           layout]}
   col-i row-i]
  (when (and (<= 0 col-i)
             (<= 0 row-i (dec nr-lines)))
    (str (get-in parsed-input [:layout row-i col-i])
         (pick-diagonal parsed-input (dec col-i) (inc row-i)))))

(defn diagonals
  [{:as   parsed-input
    :keys [width
           nr-lines
           layout]}]

  (vec (->> (concat (for [col (range width)]
                      (pick-diagonal parsed-input col 0))
                    (for [row (range 1 nr-lines)]
                      (pick-diagonal parsed-input (dec width) row)))
            (filter (comp pos? #(- % 3) count)))))

(defn diagonals-in-reverse
  [parsed-input]
  (mapv (comp #(apply str %) reverse) (diagonals parsed-input)))

;;;;;;;;;;;;;;;;;;;;

(defn pick-counter-diagonal
  "Goes down and left until out-of-bounds"
  [{:as   parsed-input
    :keys [nr-lines
           width
           layout]}
   col-i row-i]
  (when (and (<= 0 col-i (dec width))
             (<= 0 row-i (dec nr-lines)))
    (str (get-in parsed-input [:layout row-i col-i])
         (pick-counter-diagonal parsed-input (inc col-i) (inc row-i)))))

(defn counter-diagonals
  [{:as   parsed-input
    :keys [width
           nr-lines
           layout]}]

  (vec (->> (concat (for [row (range (dec nr-lines) 0 -1)]
                      (pick-counter-diagonal parsed-input 0 row))
                    (for [col (range width)]
                      (pick-counter-diagonal parsed-input col 0))
                    )
            (filter (comp pos? #(- % 3) count)))))

(defn counter-diagonals-in-reverse
  [parsed-input]
  (mapv (comp #(apply str %) reverse) (counter-diagonals parsed-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def xmas-re #"XMAS")

(defn count-all-occurences-of-xmas
  [parsed-input]
  (reduce +
          (for [line (concat (rows                         parsed-input)
                             (rows-in-reverse              parsed-input)
                             (columns                      parsed-input)
                             (columns-in-reverse           parsed-input)
                             (diagonals                    parsed-input)
                             (diagonals-in-reverse         parsed-input)
                             (counter-diagonals            parsed-input)
                             (counter-diagonals-in-reverse parsed-input))]
            (count (re-seq xmas-re line)))))


(comment

  (rows (parse-input input-1))
  (rows-in-reverse (parse-input input-1))
  (columns (parse-input input-1))
  (columns-in-reverse (parse-input input-1))

  (diagonals (parse-input input-1))
  (counter-diagonals (parse-input input-1))

  (count-all-occurences-of-xmas (parse-input input-1))
  (count-all-occurences-of-xmas (parse-input (-common/day-input 2024 4))) 

  )

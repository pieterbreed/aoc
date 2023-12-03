(ns aoc.2023.day3
  (:require [aoc.common :as -common]
            [clojure.string :as str]))

(def example-schematic
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def starts-with-digit-sequence #"(\d+)(\D.*|$)")

(defn line->digit-sequences+column
  "Reads a single line, returns tuples.
  Tuples are [<digit-sequence> <column-of-first-digit>]"
  [line]
  (loop [r   line
         col 0
         acc []]
    (if (zero? (count r))
      acc
      (let [[_ s] (re-matches starts-with-digit-sequence r)
            advance (if s
                      (inc (count s))
                      1)]
        (recur (subs r (min (count r)
                            advance))
               (+ col advance)
               (if s
                 (conj acc [s col])
                 acc))))))

(comment

  (line->digit-sequences+column "...123...") ;; [["123" 3]]
  (line->digit-sequences+column "123...") ;; [["123" 0]]
  (line->digit-sequences+column "123...123") ;;[["123" 0] ["123" 6]]
  (line->digit-sequences+column "123...123oeuCOEU13...") ;; [["123" 0] ["123" 6] ["13" 16]]

  (for [l (str/split-lines example-schematic)]
    (line->digit-sequences+column l))
  '([["467" 0] ["114" 5]]
    []
    [["35" 2] ["633" 6]]
    []
    [["617" 0]]
    [["58" 7]]
    [["592" 2]]
    [["755" 6]]
    []
    [["664" 1] ["598" 5]])

  )

(defn schematic->digit-sequences+coords
  "Reads an engine schematic, returns a seq of tuples.
  Tuples are [<digit-sequence> <co-ord of first digit>]"
  [schematic]
  (->> schematic
       (str/split-lines)
       (map-indexed (fn [line-nr line]
                      [line-nr (line->digit-sequences+column line)]))
       (filter (comp seq second))
       (mapcat (fn [[line-nr digit-sequence-column-tuples]]
                 (map (fn [[digit-sequence column]]
                        [digit-sequence [line-nr column]])
                      digit-sequence-column-tuples)))))

(comment

  (schematic->digit-sequences+coords example-schematic)
  '(["467" [0 0]]
    ["114" [0 5]]
    ["35" [2 2]]
    ["633" [2 6]]
    ["617" [4 0]]
    ["58" [5 7]]
    ["592" [6 2]]
    ["755" [7 6]]
    ["664" [9 1]]
    ["598" [9 5]])

  )

(defn coords-adjacent-to-digit-sequence
  "Computes a seq of coords that are adjacent to a [digit-sequence [row column]] tuple."
  [[digit-sequence [row column]]]
  (concat (for [c (range (dec column)
                         (+ 1
                            column
                            (count digit-sequence)))]
            [(dec row) c])
          [[row (dec column)]
           [row (+ column
                   (count digit-sequence))]]
          (for [c (range (dec column)
                         (+ 1
                            column
                            (count digit-sequence)))]
            [(inc row) c])))

(comment

  (coords-adjacent-to-digit-sequence ["467" [0 0]])
  '([-1 -1] [-1 0] [-1 1] [-1 2] [-1 3]
    [0 -1]                       [0 3]
    [1 -1]  [1 0]  [1 1]  [1 2]  [1 3])

  (coords-adjacent-to-digit-sequence ["35" [2 2]])
  '([1 1] [1 2] [1 3] [1 4]
    [2 1]             [2 4]
    [3 1] [3 2] [3 3] [3 4])

  (coords-adjacent-to-digit-sequence ["633" [2 6]])
  '([1 5] [1 6] [1 7] [1 8] [1 9]
    [2 5]                   [2 9]
    [3 5] [3 6] [3 7] [3 8] [3 9])

  (coords-adjacent-to-digit-sequence ["755" [7 6]])
  '([6 5] [6 6] [6 7] [6 8] [6 9]
    [7 5]                   [7 9]
    [8 5] [8 6] [8 7] [8 8] [8 9])

  )

(defn make-schematic-lookup
  "Takes a schematic and returns a fn `schematic-lookup` that returns the char at a coord."
  [schematic]
  (let [rows (str/split-lines schematic)]
    (fn schematic-lookup
      [[row column :as coord]]
      (cond
        ;; negatively out of bounds
        (or (neg? row)
            (neg? column))
        nil

        ;; positively out of bounds
        (or (>= row (count rows))
            (>= column (count (get rows row))))
        nil

        :else
        (get-in rows coord)))))

(defn make-is-symbol?
  "Takes a schematic, and returns a fn `is-symbol?` that can check if there is a symbol at specific coordinate."
  [schematic]
  (let [lookup (make-schematic-lookup schematic)]
    (fn is-symbol? [coord]
      (let [x (lookup coord)]
        (if-not x
          false
          (not (or (= \. x)
                   (Character/isDigit x))))))))

(comment

  (def is-symbol? (make-is-symbol? example-schematic))

  (is-symbol? [-1 -1]) ;; false
  (is-symbol? [2 10]) ;; false

  (is-symbol? [1 4]) ;; false
  (is-symbol? [1 3]) ;; true
  (is-symbol? [1 2]) ;; false

  (is-symbol? [2 6]) ;; false
  (is-symbol? [2 7]) ;; false

  (is-symbol? [3 6]) ;; true
  (is-symbol? [3 7]) ;; false

  (is-symbol? [4 3]) ;; true
  (is-symbol? [5 5]) ;; true

  )

(defn schematic->part-numbers
  "Reads a schematic and returns a seq of all of the part numbers found.

  Parts numbers are digit-sequences, that are also adjacent to symbols.
  a `.` is not a symbol."
  [schematic]
  (let [digit-seqs-coords (schematic->digit-sequences+coords schematic)
        is-symbol? (make-is-symbol? schematic)]
    (->> digit-seqs-coords
         (map (juxt identity coords-adjacent-to-digit-sequence))
         (filter (fn [[_ coords-seq]] (some is-symbol? coords-seq)))
         (map ffirst))))

(comment

  (schematic->part-numbers example-schematic)

  )

(defn solution-1
  [schematic]
  (let [part-numbers (schematic->part-numbers schematic)]
    (reduce + (map #(Long/parseLong %) part-numbers))))

(comment

  (solution-1 example-schematic) ;; 4361
  (solution-1 (-common/day-input 2023 3))

  )

(defn make-is-gear?
  "Takes a schematic, and returns a fn `is-symbol?` that can check if there is a symbol at specific coordinate."
  [schematic]
  (let [lookup (make-schematic-lookup schematic)]
    (fn is-gear? [coord]
      (if-let [x (lookup coord)]
        (= \* x)
        false))))

(defn solution-2
  [schematic]
  (let [digit-seqs-coords (schematic->digit-sequences+coords schematic)
        is-gear? (make-is-gear? schematic)

        gears-adjacent-to-parts
        (->> digit-seqs-coords
             (map (juxt identity coords-adjacent-to-digit-sequence))
             (map (fn [[digit-seq adjacent-coords]]
                    [(first digit-seq)
                     (filter is-gear?
                             adjacent-coords)]))
             (filter (comp seq second))
             (mapcat (fn [[part-nr adjacent-gears]]
                       (map #(vector % part-nr) adjacent-gears)))
             (group-by first)
             (map (fn [[coord ss]] [coord (map second ss)]))
             (doall))]

    ;; check for an assumption about a gear being adjecent to only 2 part numbers
    (doseq [[gear-coord part-nrs] gears-adjacent-to-parts]
      (when (< 2 (count part-nrs))
        (throw (ex-info "Unexpected situation with one gear adjacent to more than 1 part number."
                        {::gear-coord gear-coord
                         ::part-nrs part-nrs}))))

    (->> gears-adjacent-to-parts
         (filter (comp #(= 2 (count %)) second))
         (map second)
         (map (fn [part-numbers] (map #(Long/parseLong %) part-numbers)))
         (map #(reduce * %))
         (reduce +))))

(comment

  (solution-2 example-schematic) ;; 467835
  (solution-2 (-common/day-input 2023 3))


  )

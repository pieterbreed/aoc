(ns aoc.2023.day12
  (:require [aoc.common :as -common]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [clojure.core.async :as csp]
            [clojure.set :as set]))

(def example-input-1
  "#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1")

(def example-input-2
  "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (mapv (fn [[springs-row lengths-list]]
               {:parts (mapv (fn [ch]
                               (case ch
                                 \. :.
                                 \# :#
                                 \? :?))
                             springs-row)
                :checksum (mapv #(Long/parseLong %)
                                (str/split lengths-list #","))}))))

(comment

  (parse-input example-input-1)
  )

(defn parsed-row->row-possibilities
  [[current-part & rest-of-the-parts :as _parts-list]]
  (let [single-calc-children-possibilities (memoize
                                            #(parsed-row->row-possibilities rest-of-the-parts))]
    (cond
      ;; should hopefully never be true
      (nil? current-part)
      []

      ;; current-part is not-nil
      ;; terminate recursion here
      (nil? rest-of-the-parts)
      (if (= current-part :?)
        [[:.] [:#]]
        [[current-part]])

      (not= current-part :?)
      (for [possibility (single-calc-children-possibilities)]
        (concat [current-part] possibility))

      (= current-part :?)
      (for [option      #{:# :.}
            possibility (single-calc-children-possibilities)]
        (concat [option] possibility)))))



(comment

  {:parts [:? :? :? :. :# :# :#], :checksum [1 1 3]}
  {:parts [:. :? :? :. :. :? :? :. :. :. :? :# :# :.], :checksum [1 1 3]}
  {:parts [:? :# :? :# :? :# :? :# :? :# :? :# :? :# :?], :checksum [1 3 1 6]}
  {:parts [:? :? :? :? :. :# :. :. :. :# :. :. :.], :checksum [4 1 1]}
  {:parts [:? :? :? :? :. :# :# :# :# :# :# :. :. :# :# :# :# :# :.], :checksum [1 6 5]}
  {:parts [:? :# :# :# :? :? :? :? :? :? :? :?], :checksum [3 2 1]}

  (parsed-row->row-possibilities [:? :? :? :. :# :# :#])

  ;;  {(:# :# :# :. :# :# :#)
  ;;   (:# :# :. :. :# :# :#)
  ;;   (:# :. :# :. :# :# :#)
  ;;   (:# :. :. :. :# :# :#)
  ;;   (:. :# :# :. :# :# :#)
  ;;   (:. :# :. :. :# :# :#)
  ;;   (:. :. :# :. :# :# :#)
  ;;   (:. :. :. :. :# :# :#)}

  )

(defn partslist->checksum
  [partslist]
  (let [{:keys [checksum
                counted-parts]
         :or {counted-parts 0}}
        (reduce (fn [{checksum' :checksum
                      counted-parts' :counted-parts
                      :or       {counted-parts' 0}
                      :as       acc}
                     x]
                  (case x
                    :# (assoc acc :counted-parts (inc counted-parts'))
                    :. (if (zero? counted-parts')
                         acc
                         {:counted-parts 0
                          :checksum      (conj checksum' counted-parts')})))
                {:checksum []}
                partslist)]
    (if (zero? counted-parts)
      checksum
      (conj checksum counted-parts))))

(comment

  (partslist->checksum (list :# :# :# :. :# :# :#)) ;; [3 3]
  (partslist->checksum (list :# :. :# :. :# :# :#)) ;; [1 1 3]

  #_[(:# :# :# :. :# :# :#)
   (:# :# :. :. :# :# :#)
   (:# :. :# :. :# :# :#)
   (:# :. :. :. :# :# :#)
   (:. :# :# :. :# :# :#)
   (:. :# :. :. :# :# :#)
   (:. :. :# :. :# :# :#)
   (:. :. :. :. :# :# :#)]

  )


(defn solution-1
  [input]
  (let [records (parse-input input)]
    (->> (for [{:keys [checksum parts]}   records
               :let [all-possibilities (parsed-row->row-possibilities parts)]]
           (->> all-possibilities
                (filter #(= checksum (partslist->checksum %)))
                (count)))
         (reduce +))))


(comment

  (solution-1 example-input-2) ;; 21
  (time (solution-1 (-common/day-input 2023 12)))
  ;; "Elapsed time: 20393.964303 msecs"

  )

(defn unfold-records
  [records times]
  (->> records
       (mapv (fn [{:keys [parts checksum]}]
               {:parts (->> (constantly parts)
                            (repeatedly times)
                            (reduce (fn [parts1 parts2]
                                      (concat parts1
                                              [:?]
                                              parts2)))
                            (vec))
                :checksum (vec (take (* times (count checksum))
                                     (cycle checksum)))}))))

(defn count-row-possibilities
  [[current-part & rest-of-the-parts :as _parts-list]]
  (let [single-calc-children-possibilities (memoize
                                            #(count-row-possibilities rest-of-the-parts))]
    (cond
      ;; current-part is not-nil
      ;; terminate recursion here
      (nil? rest-of-the-parts)
      (if (= current-part :?)
        2
        1)

      (not= current-part :?)
      (single-calc-children-possibilities)

      (= current-part :?)
      (* 2 (single-calc-children-possibilities)))))

(comment

  (count-row-possibilities [:. :? :? :. :. :? :? :. :. :. :? :# :# :.])

  )



(defn partslist->checksum->possible?
  "Checks if the partslist has the checksum, or if it may have the checksum (if the partslist is too small)."
  [parts checksum]

  (let [local-checksum (partslist->checksum parts)]
    (cond
      ;; if the local checksum has more numbers in the checksum, then it cannot match
      (> (count local-checksum)
         (count checksum))
      false

      ;; if they have the same number
      ;; then all but the last must be exactly the same
      ;; and the local checksum's last number must be <= actual checksum's last number
      (= (count local-checksum)
         (count checksum))
      (and (= (butlast local-checksum)
              (butlast checksum))
           (<= (last local-checksum)
               (last checksum)))

      ;; if the local checksum has fewer numbers
      (< (count local-checksum)
         (count checksum))
      (let [checksum:as-many (take (count local-checksum) checksum)]
        (and (= (butlast local-checksum)
                (butlast checksum:as-many))
             (<= (or (last local-checksum) 0)
                 (or (last checksum:as-many) 0)))))))

(comment

  (for [l (parse-input example-input-1)]
    (partslist->checksum->possible? (:parts l) (:checksum l)))

  (partslist->checksum->possible? [:# :. :# :. :# :# :#]
                                  [1 1 3])

  )

(defn parsed-row->row-possibilities-2
  "We maintain the interface of parsed-row->row-possibilities
  but hopefully never generate invalid options."
  [{[current-part & rest-of-the-parts] :parts
    :keys [checksum]}
   & {:keys [accumulated-parts-list]
      :or   {accumulated-parts-list []}}]
  (let [next-part-possibilities
        (cond

          (nil? current-part)
          [[]]

          (= current-part :?)
          [[:.] [:#]]

          :else
          [[current-part]])

        worthwhile-possibilities
        (->> next-part-possibilities
             (map (partial concat accumulated-parts-list))
             (filterv #(partslist->checksum->possible? % checksum)))]

    (if-not rest-of-the-parts
      (filterv #(= checksum (partslist->checksum %))
               worthwhile-possibilities)
      (mapcat #(parsed-row->row-possibilities-2 {:parts    rest-of-the-parts
                                                 :checksum checksum}
                                                :accumulated-parts-list %)
       worthwhile-possibilities))))

(comment

  (parsed-row->row-possibilities-2 {:parts [:? :? :? :. :# :# :#], :checksum [1 1 3]})
  (parsed-row->row-possibilities-2 {:parts [:. :? :? :. :. :? :? :. :. :. :? :# :# :.], :checksum [1 1 3]})
  (parsed-row->row-possibilities-2 {:parts [:? :# :? :# :? :# :? :# :? :# :? :# :? :# :?], :checksum [1 3 1 6]})

  (parsed-row->row-possibilities-2 {:parts [:? :? :? :? :. :# :. :. :. :# :. :. :.], :checksum [4 1 1]})
  (parsed-row->row-possibilities-2 {:parts [:? :? :? :? :. :# :# :# :# :# :# :. :. :# :# :# :# :# :.], :checksum [1 6 5]})
  (parsed-row->row-possibilities-2 {:parts [:? :# :# :# :? :? :? :? :? :? :? :?], :checksum [3 2 1]})

  {:parts [:? :? :? :. :# :# :#], :checksum [1 1 3]}
  {:parts [:. :? :? :. :. :? :? :. :. :. :? :# :# :.], :checksum [1 1 3]}
  {:parts [:? :# :? :# :? :# :? :# :? :# :? :# :? :# :?], :checksum [1 3 1 6]}
  {:parts [:? :? :? :? :. :# :. :. :. :# :. :. :.], :checksum [4 1 1]}
  {:parts [:? :? :? :? :. :# :# :# :# :# :# :. :. :# :# :# :# :# :.], :checksum [1 6 5]}
  {:parts [:? :# :# :# :? :? :? :? :? :? :? :?], :checksum [3 2 1]}

  )

(defn solution-2
  [input & {:keys [scale]
            :or   {scale 1}}]
  (let [records (parse-input input)
        records (unfold-records records scale)]
    (->> (for [record   records
               :let [valid-possibilities (parsed-row->row-possibilities-2 record)]]
           (count valid-possibilities))
         (reduce +))))

(defn solution-2-2
  [input & {:keys [scale]
            :or   {scale 1}}]
  (let [records (parse-input input)
        records (unfold-records records scale)]
    (->> records
         (pmap #(count (parsed-row->row-possibilities-2 %)))
         (reduce +))))

(defn solution-2-3
  [input & {:keys [scale]
            :or   {scale 1}}]
  (let [records (parse-input input)
        records (unfold-records records scale)]
    (->> records
         (map #(csp/thread (count (parsed-row->row-possibilities-2 %))))
         (doall)
         (map #(csp/<!! %))
         (reduce +))))

(comment

  (solution-2 example-input-2) ;; 21
  (time (solution-2 example-input-2 :scale 5)) ;; 525152
  ;; "Elapsed time: 116613.233741 msecs"

  (solution-2-2 example-input-2) ;; 21
  (time (solution-2-2 example-input-2 :scale 5)) ;; 525152
  ;; "Elapsed time: 109590.860098 msecs"

  (solution-2-3 example-input-2) ;; 21
  (time (solution-2-3 example-input-2 :scale 5)) ;; 525152
  ;; "Elapsed time: 120607.892229 msecs"

  (time (solution-2 (-common/day-input 2023 12))) ;; answer for puzzle 1
  ;; "Elapsed time: 2852.983524 msecs"
  ;;
  ;; Compare with (time (solution-1 (-common/day-input 2023 12))) above
  ;; "Elapsed time: 20393.964303 msecs"
  (time (solution-2 (-common/day-input 2023 12) :scale 5))

  )

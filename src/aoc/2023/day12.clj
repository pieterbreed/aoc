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
               :let [all-possibilities (parsed-row->row-possibilities parts)
                     valid-ones (->> all-possibilities
                                     (filter #(= checksum (partslist->checksum %)))
                                     (count))]]
           [parts valid-ones])
         (map second)
         (reduce +))))


(comment

  (solution-1 example-input-2) ;; 21
  (solution-1 (-common/day-input 2023 12))

  )

(defn unfold-records
  [records times]
  (->> records
       (mapv (fn [{:keys [parts checksum]}]
               {:parts (vec (take (* times (count parts))
                                  (cycle parts)))
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

(defn solution-2
  [input]
  (let [records (parse-input input)
        records (unfold-records records 5)]
    (->> (for [{:keys [checksum parts]}   records
               :let [all-possibilities (parsed-row->row-possibilities parts)
                     valid-ones (->> all-possibilities
                                     (filter #(= checksum (partslist->checksum %)))
                                     (count))]]
           [parts valid-ones])
         (map second)
         (reduce +))))

(comment

  (solution-2 example-input-2)

  )

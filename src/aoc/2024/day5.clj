(ns aoc.2024.day5
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   [taoensso.timbre :as log]
   [clojure.set :as set]))

(def input-1 "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn contains?
  [print-selection x]
  (not= -1 (.indexOf ^java.util.List print-selection x)))

(defn before?
  "Is a before b in the print-selection?"
  [print-selection a b]
  (< (.indexOf ^java.util.List print-selection a)
     (.indexOf ^java.util.List print-selection b)))

(defn parse-input
  [input]
  (let [[order-rules-lines print-selections-lines]
        (split-with (comp pos? count)
                    (str/split-lines input))
        print-selections-lines (rest print-selections-lines)

        order-rules-data (->> order-rules-lines
                              (map #(str/split % #"\|"))
                              (-common/vector-of-strs->vector-of-long)
                              (into []))]
    
    {:order-rules-data order-rules-data
     :order-rules-fn   (apply every-pred
                              (->> order-rules-data
                                   (map (fn [[a b]]
                                          (fn [selection]
                                            (if (and (contains? selection a)
                                                     (contains? selection b))
                                              (before? selection a b)
                                              true))))))
     :print-selections (->> print-selections-lines
                            (map #(str/split % #","))
                            (-common/vector-of-strs->vector-of-long)
                            (into []))}))

(comment

  (parse-input input-1)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sol1
  [input]
  (let [{:keys [order-rules-fn
                print-selections]} (parse-input input)]
    (->> print-selections
         (filter order-rules-fn)
         (map #(get % (quot (count %) 2)))
         (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare items-before-x)
(def items-before-x
  (memoize (fn [{:keys [order-rules-data]} x]
             (let [get-immediately-before (memoize
                                           (fn [x]
                                             (->> order-rules-data
                                                  (filter (comp #(= % x) second))
                                                  (mapv first))))
                   all-previous-lookups (loop [collected (set (get-immediately-before x))]
                                          (let [new-collected (reduce set/union
                                                                     collected
                                                                     (map (comp set get-immediately-before) collected))]
                                            (if (= new-collected
                                                   collected)
                                              collected
                                              (recur new-collected))))]

               (vec all-previous-lookups)))))

(comment

  (items-before-x (parse-input input-1) 29) ;;  ;; #{75 61 47 97 53}
  (items-before-x (parse-input input-1) 13) ;;  ;; #{75 61 29 47 97 53}

  )

(defn parsed-input->comparitor
  [parsed-input]
  (fn [a b]
    (let [before-a? (set (items-before-x parsed-input a))
          before-b? (set (items-before-x parsed-input b))]
      (cond
        (before-b? a) -1
        (before-a? b) 1
        :else         0))))

(comment

  (sol1 input-1) ;; 143
  (sol1 (-common/day-input 2024 5)) ;;3608

  
  


  )

(defn get-rules-for-selection
  [rules-data
   selection]
  (let [selection-pages (set selection)]
    (->> rules-data
         (filterv (fn [[a b]]
                    (and (selection-pages a)
                         (selection-pages b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sort:head-with-others
  [])

(defn sort:selection
  [{:as   _parsed-input
    :keys [order-rules-data]}
   selection]

  (let [;; now we are only working with rules that apply to this selection
        selection-rules (get-rules-for-selection order-rules-data selection)

        ;; we will fix this by looking from the head-pagen-number to the rest for violations
        ;; if we find a violation we fix it by switching

        find-and-fix-violations (fn find-and-fix-violations
                                  [selection']
                                  (let [selection'-rules (get-rules-for-selection selection-rules
                                                                                  selection')

                                        [head-page-nr & remaining-page-numbers] selection'
                                        pages-that-must-be-before-head-page-nr (->> selection'-rules
                                                                                    (map first)
                                                                                    (set))

                                        selection:after-first-fix (first (for [[page-index page-nr] (map-indexed #(vector (inc %1) ;; index into the rest of the page selection, increase to find index into original selection
                                                                                                                          %2)
                                                                                                                 remaining-page-numbers)
                                                                               :when (pages-that-must-be-before-head-page-nr page-nr)]
                                                                           (-> (vec selection')
                                                                               ;; we're about to swap the head with the offending item
                                                                               (assoc 0 page-nr)               ;; put the one that must be before the head, into the head's place/index
                                                                               (assoc page-index head-page-nr) ;; put the current head, into the place of the one that must be before it
                                                                               )))]

                                    ;; if the head is ok, ie no changes were made
                                    ;; then we can recurse into the `remaining-page-numbers`
                                    ;; with what we have, otherwise we must restart at the new head
                                    (if selection:after-first-fix
                                      ;; we must restart the sequence of fixes the current new head after the fix
                                      ;; but don't increase the stack, just remaing on the current stack frame by recurring
                                      (recur selection:after-first-fix)

                                      ;; return a vector with head-page-nr being the first item
                                      ;; after which follows the remaining-page-numbers with fixes
                                      ;; we can afford one stack item per items in the selection
                                      (vec (concat [head-page-nr]
                                                   (find-and-fix-violations remaining-page-numbers))))))]
    (find-and-fix-violations selection)))

(comment

  

  (let [pi (parse-input input-1)
        failing-case [[75 97 47 61 53]
                      [61 13 29]
                      [97 13 75 29 47]]]
    (sort:selection pi (first failing-case)))
  


  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sol2
  [input]
  (let [{:as   pi
         :keys [order-rules-fn
                print-selections]} (parse-input input)
        cp (parsed-input->comparitor pi)

        fixed-selections
        (->> print-selections
             (filter (complement order-rules-fn))
             (map (fn [x]
                    (loop [current-order x]
                      (let [new-order (vec (sort-by identity cp current-order))]
                        (if (= current-order new-order)
                          current-order
                          (recur new-order))))))
             (map vec))

        changed (for [s fixed-selections
                      :when (not= s (vec (sort-by identity cp s)))]
                  s)

        still-fixed (->> fixed-selections
                         (filter order-rules-fn))
        x (= (count fixed-selections)
             (count still-fixed))]
    (reduce +
            (->> fixed-selections
                 (map #(get % (quot (count %) 2)))))))

(comment

  (sol2 input-1) ;; 123
  (sol2 (-common/day-input 2024 5)) ;; 5326

  )

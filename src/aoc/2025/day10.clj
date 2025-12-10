(ns aoc.2025.day10
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]
   [net.cgrand.xforms :as x]
   [clojure.java.io :as io]
   ))

(def test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn parse-line-parts
  [[lights & btns-and-joltage]]
  (let [lights-raw (as-> lights $
                     (subs $ 1 (dec (count lights))))
        lights'' (mapv (fn [ch] (if (= ch \.) false true)) lights-raw)
        buttons' (->> btns-and-joltage
                      (butlast)
                      (mapv (fn [btn-wire]
                              (let [btns' (subs btn-wire 1 (dec (count btn-wire)))]
                                (mapv Long/parseLong (str/split btns' #","))))))
        joltage' (as-> (last btns-and-joltage) $
                   (subs $ 1 (dec (count $)))
                   (str/split $ #",")
                   (mapv Long/parseLong $))]
    {:desired-lights lights''
     :buttons        buttons'
     :joltage        joltage'}))

(comment

  (parse-line-parts ["[.##.]" "(3)" "(1,3)" "(2)" "(2,3)" "(0,2)" "(0,1)" "{3,5,4,7}"])

  )

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #" "))
       (map parse-line-parts)))

(comment

  (parse-input test-input)


  )

(defn make-press-button
  [{:as _button-config :keys [buttons]}]
  (fn [state button-i]
    (reduce (fn [state light-i] (update state light-i not))
            state
            (get buttons button-i))))

(defn make-blank-state
  [{:as _button-config :keys [desired-lights]}]
  (into [] (take (count desired-lights) (repeat false))))

(defn press-buttons
  [btn-config button-presses]
  (let [presser     (make-press-button btn-config)
        blank-state (make-blank-state btn-config)]
    (reduce presser
            blank-state
            button-presses)))

(comment

  (let [btn-config (parse-line-parts ["[.##.]" "(3)" "(1,3)" "(2)" "(2,3)" "(0,2)" "(0,1)" "{3,5,4,7}"])]
    (press-buttons btn-config [1 3 4 4]))

  )

(defn seek-min-button-presses
  [{:as btn-config :keys [buttons
                          desired-lights]}]
  (let [presser      (make-press-button btn-config)
        button-is    (range (count buttons))]

    (loop [state-queue        (conj clojure.lang.PersistentQueue/EMPTY
                                    {:state   (make-blank-state btn-config)
                                     :presses []})
           encountered-states #{}]
      (let [{:as   _current-state
             :keys [state
                    presses]} (peek state-queue)
            remaining-states  (pop state-queue)
            encountered-states (conj encountered-states state)]

        (if (= desired-lights state) presses
            (recur (let [new-states
                         (->> button-is
                              (map (fn [button-index]
                                     {:state   (presser state button-index)
                                      :presses (conj presses button-index)}))
                              (filter (fn [{:keys [state]}] (not (encountered-states state)))))]
                     (reduce conj remaining-states new-states))
                   encountered-states))))))

(comment

  (let [btn-config (parse-line-parts ["[.##.]" "(3)" "(1,3)" "(2)" "(2,3)" "(0,2)" "(0,1)" "{3,5,4,7}"])]

    (seek-min-button-presses btn-config)

    )

  (seek-min-button-presses {:desired-lights [false false false true false],
                            :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]],
                            :joltage [7 5 12 7 2]})

  (seek-min-button-presses {:desired-lights [false true true true false true],
                            :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]],
                            :joltage [10 11 11 5 10 5]})

  )

(defn solve-1
  [input]
  (let [input (parse-input input)]
    (->> input
         (map seek-min-button-presses)
         (map count)
         (reduce +))))

(comment

  (solve-1 test-input) ;; 7
  (solve-1 (-common/day-input 2025 10)) ;; 520

  )

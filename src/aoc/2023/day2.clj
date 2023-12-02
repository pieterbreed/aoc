(ns aoc.2023.day2
  (:require [aoc.common :as -common]
            [instaparse.core :as ip]
            [clojure.set :as set]
            [clojure.string :as str]))

(def game-record-parser
  (ip/parser
   "<document> = line (<'\n'> line)+
    line = <'Game'> <whitespace> number <':'> <whitespace> set (<';'> <whitespace> set)*
    set = number-of-color (<','> <whitespace> number-of-color)*
    <number-of-color> = number <whitespace> color
    <whitespace> = #'\\s+'
    number = #'[0-9]+'
    <color> = 'red' | 'green' | 'blue'"))

(def test-input-1
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(comment

  (ip/parse game-record-parser
            test-input-1)

  '([:line [:number "1"] [:set [:number "3"] "blue" [:number "4"] "red"] [:set [:number "1"] "red" [:number "2"] "green" [:number "6"] "blue"] [:set [:number "2"] "green"]]
    [:line [:number "2"] [:set [:number "1"] "blue" [:number "2"] "green"] [:set [:number "3"] "green" [:number "4"] "blue" [:number "1"] "red"] [:set [:number "1"] "green" [:number "1"] "blue"]]
    [:line [:number "3"] [:set [:number "8"] "green" [:number "6"] "blue" [:number "20"] "red"] [:set [:number "5"] "blue" [:number "4"] "red" [:number "13"] "green"] [:set [:number "5"] "green" [:number "1"] "red"]]
    [:line [:number "4"] [:set [:number "1"] "green" [:number "3"] "red" [:number "6"] "blue"] [:set [:number "3"] "green" [:number "6"] "red"] [:set [:number "3"] "green" [:number "15"] "blue" [:number "14"] "red"]]
    [:line [:number "5"] [:set [:number "6"] "red" [:number "1"] "blue" [:number "3"] "green"] [:set [:number "2"] "blue" [:number "1"] "red" [:number "2"] "green"]])


  )

(defn parse-game-record
  "Takes the game record (eg see `test-input-1`)

  Parses it,
  does a small transform,
  and returns a data structure representing the content."
  [game-record-str]
  (->> game-record-str
       (str/trim)
       (ip/parse game-record-parser)
       (ip/transform {:number #(Long/parseLong %)
                      :set (fn [& contents]
                             (->> contents
                                  (partition 2)
                                  (map #(vector (second %) (first %)))
                                  (map vec)
                                  (into {})))
                      :line (fn [line-nr & sets]
                              [line-nr (vec sets)])})))

(comment

  (parse-game-record test-input-1)

  '([1 [{"blue" 3, "red" 4} {"red" 1, "green" 2, "blue" 6} {"green" 2}]]
    [2 [{"blue" 1, "green" 2} {"green" 3, "blue" 4, "red" 1} {"green" 1, "blue" 1}]]
    [3 [{"green" 8, "blue" 6, "red" 20} {"blue" 5, "red" 4, "green" 13} {"green" 5, "red" 1}]]
    [4 [{"green" 1, "red" 3, "blue" 6} {"green" 3, "red" 6} {"green" 3, "blue" 15, "red" 14}]]
    [5 [{"red" 6, "blue" 1, "green" 3} {"blue" 2, "red" 1, "green" 2}]])

  )

(defn possible-result?
  "Takes a game result (eg {3 \"blue\" 4 \"red})
  and a bag's content (eg {\"red\" 12 \"green\" 13 \"blue\" 14})
  Returns the game result IFF the game result is <= for each color cube."
  [bag-content game-result]
  (when (->> game-result
             (keep (fn [[color nr]]
                     (when (> nr (get bag-content color))
                       :impossible)))
             (count)
             (zero?))
    game-result))

(comment

  (possible-result?
   {"blue" 3 "green" 2}
   {"blue" 3 "green" 2})

  (possible-result?
   {"blue" 4 "green" 5}
   {"blue" 3 "green" 2})

  (possible-result?
   {"blue" 2 "green" 2}
   {"blue" 3 "green" 2})

  (possible-result?
   {"blue" 3 "green" 1}
   {"blue" 3 "green" 2})

  )

(defn solution-1
  [bag-contents game-result]
  (let [parsed (parse-game-record game-result)
        invalid-game-nrs (set
                          (for [[game-nr sets] parsed
                                game-set sets
                                :let [possible? (possible-result? bag-contents game-set)]
                                :when (not possible?)]
                            game-nr))
        all-game-nrs (set (map first parsed))]
    (reduce + (set/difference all-game-nrs
                              invalid-game-nrs))))

(comment

  (solution-1 {"red" 12
               "green" 13
               "blue" 14}
              test-input-1) ;; 8
  (solution-1 {"red" 12
               "green" 13
               "blue" 14}
              (-common/day-input 2023 2))

  )

(defn power-of-set [set] (reduce * (vals set)))

(comment

  (power-of-set {"red" 4
                 "green" 2
                 "blue" 6})

  )

(defn solution-2
  [game-result]
  (let [parsed (parse-game-record game-result)]
    (->> parsed
         (map second)
         (map (fn [sets] (apply merge-with max sets)))
         (map power-of-set)
         (reduce +))))

(comment

  (solution-2 test-input-1)

  (solution-2 (-common/day-input 2023 2))

  )

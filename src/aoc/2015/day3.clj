(ns aoc.2015.day3
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))

(defn solve-1
  [input]
  (let [start [0 0]
        input' (map (fn [ch]
                      (case ch
                        \^ (fn [[x y]] [x (inc y)])
                        \v (fn [[x y]] [x (dec y)])
                        \< (fn [[x y]] [(dec x) y])
                        \> (fn [[x y]] [(inc x) y])))
                    input)

        result
        (reduce (fn [{:keys [current history] :as acc} modf]
                  (let [next (modf current)]
                    (-> acc
                        (assoc :current next)
                        (update :history conj next))))
                {:current start
                 :history #{start}}
                input')]
    (count (:history result))))

(comment

  (solve-1 "><><^vv") ;; 4
  (solve-1 ">") ;; 2
  (solve-1 "^>v<") ;; 4
  (solve-1 "^v^v^v^v^v") ;; 2

  (solve-1 (-common/day-input 2015 3)) ;;

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn solve-2
  [input]
  (let [input' (map (fn [ch]
                      (case ch
                        \^ (fn [[x y]] [x (inc y)])
                        \v (fn [[x y]] [x (dec y)])
                        \< (fn [[x y]] [(dec x) y])
                        \> (fn [[x y]] [(inc x) y])))
                    input)

        [santas-instructions
         robos-instructions
         :as _all-instructions] (->> input'
                                     (partition-all 2)
                                     (reduce (fn [[santa robo] [santa-in robo-in]]
                                               [(conj santa santa-in)
                                                (conj robo  robo-in)])
                                             [[] []]))

        walker  (fn [start* input*]
                  (reduce (fn [{:keys [current] :as acc} modf]
                            (let [next (modf current)]
                              (-> acc
                                  (assoc :current next)
                                  (update :history conj next))))
                          {:current start*
                           :history #{start*}}
                          input*))

        santas-result (walker [0 0] santas-instructions)
        robo-result (walker [0 0] robos-instructions)]
    (count (set/union (:history santas-result)
                      (:history robo-result)))))

(comment

  (solve-2 "^v") ;; 3
  (solve-2 "^>v<") ;; 3
  (solve-2 "^v^v^v^v^v") ;; 11
  (solve-2 (-common/day-input 2015 3)) ;;

  )

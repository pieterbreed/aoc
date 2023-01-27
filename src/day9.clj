(ns day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clj-http.client :as http]
            [clojure.set :as set]
            [clojure.test :as t :refer [deftest is]]
            [odoyle.rules :as o]
            [aoc]))

(def rope-segment-existence-rule
  (o/->rule
   ::rope-segment
   {:what '[[segment ::segment-id sid {:then not=}]
            [segment ::x          x   {:then not=}]
            [segment ::y          y   {:then not=}]]
    :then
    (fn [session {:keys [x y segment-id]}]
      (tap> [:segment segment-id :assumes :new :position x y]))}))

(defn rope-segment-following-rules
  [& {:keys [segment-id
             upstream-segment-id]}]
  (let [following-what
        '[[segment ::segment-id sid]
          [segment ::x x {:then not=}]
          [segment ::y y {:then not=}]
          [segment ::upstream-segment-id usid]
          [upstream ::segment-id usid]
          [upstream ::x u-x {:then false}]
          [upstream ::y u-y {:then false}]]

        invalid-in-direction?
        (fn [direction?]
          (fn [session {:keys [  x   y
                               u-x u-y]}]
            (and (not (aoc/d9-h-and-t-valid? [u-x u-y]
                                             [  x  y]))
                 (direction? [u-x u-y]
                             [  x   y]))))]
    [;; primary directions
     (o/->rule
      ::rope-segment-follows-upstream-north
      {:what following-what
       :when (invalid-in-direction? aoc/d9-north-of?)
       :then (fn [session {:keys [segment y]}]
               (tap> [:segment segment :shifts :north])
               (o/insert session {::y (dec y)})
               (o/reset!))})
     (o/->rule
      ::rope-segment-follows-upstream-south
      {:what following-what
       :when (invalid-in-direction? aoc/d9-south-of?)
       :then (fn [session {:keys [segment y]}]
               (tap> [:segment segment :shifts :south])
               (o/insert session {::y (inc y)})
               (o/reset!))})
     (o/->rule
      ::rope-segment-follows-upstream-east
      {:what following-what
       :when (invalid-in-direction? aoc/d9-east-of?)
       :then (fn [session {:keys [segment x]}]
               (tap> [:segment segment :shifts :east])
               (o/insert session {::x (inc x)})
               (o/reset!))})
     (o/->rule
      ::rope-segment-follows-upstream-west
      {:what following-what
       :when (invalid-in-direction? aoc/d9-west-of?)
       :then (fn [session {:keys [segment x]}]
               (tap> [:segment segment :shifts :west])
               (o/insert session {::x (dec x)})
               (o/reset!))})

     ;; complicated directions
     (o/->rule
      ::rope-segment-follows-northeast
      {:what following-what
       :when (invalid-in-direction? aoc/d9-northeast-of?)
       :then (fn [session {:keys [segment x y]}]
               (tap> [:segment segment :shifts :northeast])
               (-> session
                   (o/insert segment {::x (inc x)})
                   (o/insert segment {::y (dec y)})
                   (o/reset!)))})
     (o/->rule
      ::rope-segment-follows-northwest
      {:what following-what
       :when (invalid-in-direction? aoc/d9-northwest-of?)
       :then (fn [session {:keys [segment x y]}]
               (tap> [:segment segment :shifts :northwest])
               (-> session
                   (o/insert segment {::x (dec x)})
                   (o/insert segment {::y (dec y)})
                   (o/reset!)))})
     (o/->rule
      ::rope-segment-follows-southeast
      {:what following-what
       :when (invalid-in-direction? aoc/d9-southeast-of?)
       :then (fn [session {:keys [segment x y]}]
               (tap> [:segment segment :shifts :southeast])
               (-> session
                   (o/insert segment {::x (inc x)})
                   (o/insert segment {::y (inc y)})
                   (o/reset!)))})
     (o/->rule
      ::rope-segment-follows-southwest
      {:what following-what
       :when (invalid-in-direction? aoc/d9-southwest-of?)
       :then (fn [session {:keys [segment x y]}]
               (tap> [:segment segment :shifts :southwest])
               (-> session
                   (o/insert segment {::x (dec x)})
                   (o/insert segment {::y (inc y)})
                   (o/reset!)))})]))

(defn make-rope-rules-and-facts
  [n-segments]
  (let [links
        (map vector
             (range 1 (inc n-segments))
             (concat [::head]
                     (range 1 n-segments)))]
    {:facts (concat [[::head {::x 0 ::y 0
                              ::segment-id ::head}]]
                    (->> links
                         (map (fn [[segment-id upstream-segment-id]]
                                [segment-id {::x                  0
                                             ::y                  0
                                             ::segment-id         segment-id
                                             ::upstream-parent-id upstream-segment-id}]))))
     :rules (mapcat (fn [[segment-id upstream-segment-id]]
                      (rope-segment-following-rules :segment-id          segment-id
                                                    :upstream-segment-id upstream-segment-id))
                    links)}))

(defn make-keeping-track-of-segment-positions-rule
  [segment-id]
  {:rules [(o/->rule
            ::keep-track-of-tail-positions
            {:what '[[segment ::segment-id segment-id]
                     [segment ::x x]
                     [segment ::y y]
                     [segment ::all-positions all-positions {:then false}]]

             :when
             (fn [session match]
               (= segment-id (:segment-id match)))

             :then-finally
             (fn [session]
               (let [{:keys [x y all-positions]}
                     (->> (o/query-all session ::keep-track-of-tail-positions)
                          (filter #(= segment-id (:segment-id %)))
                          (first))]
                 (tap> [:keeping :track :of :segment segment-id :position x y])
               (-> session
                   (o/insert segment-id ::all-positions (set (conj all-positions [x y])))
                   (o/reset!))))})]
   :facts [[segment-id {::all-positions #{}}]]})

(defn make-keeping-track-of-boundaries-rules
  []
  {:rules [[::keep-track-of-map-boundaries
            (o/->rule
             :what '[[::head ::x head-x {:then not=}]
                     [::head ::y head-y {:then not=}]
                     [::max ::y max-y {:then false}]
                     [::min ::y min-y {:then false}]
                     [::max ::x max-x {:then false}]
                     [::min ::x min-x {:then false}]]

             :then
             (fn [session {:keys [max-x max-y
                                  min-x min-y
                                  head-x head-y]}]
               (-> session
                   (o/insert! ::max {::y (max max-y head-y)
                                     ::x (max max-x head-x)})
                   (o/insert! ::mi {::y (min min-y  head-y)
                                    ::x (min min-x  head-x)})
                   (o/reset!))))]]
   :facts [[::max {::x  3 ::y  3}]
           [::min {::x -3 ::y -3}]]})




(comment

  (make-rope-rules-and-facts 2)

  )

(defn movement-rule
  []
  (let [what '[[::head ::x]]])
  )

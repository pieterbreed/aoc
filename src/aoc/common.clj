(ns aoc.common
  (:require [clj-http.client :as http]))

(defonce session-cookie (atom (System/getenv "AOC_COOKIE")))

(defn- day-input*
  "Retrieves the input for the year/day combo. See `day-input`."
  [year day]
  (:body (http/get (format "https://adventofcode.com/%d/day/%d/input" year day)
                   {:cookies {"session" {:value @session-cookie}}})))


(def ^{:doc      (:doc (meta #'day-input*))
       :arglists (:arglists (meta #'day-input*))}
  day-input (memoize day-input*))


(def whitespace-regex #"\p{Zs}+")

(defn vector-of-strs->vector-of-long
  ([]
   (map (fn [vstrs] (mapv Long/parseLong vstrs))))
  ([xs]
   (map (fn [xs] (mapv Long/parseLong xs)) xs)))

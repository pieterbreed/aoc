(ns aoc.common
  (:require [clj-http.client :as http]))

(defonce session-cookie (atom (System/getenv "AOC_COOKIE")))

(defn day-input*
  "Retrieves the input for the year/day combo. See `day-input`."
  [year day]
  (:body (http/get (format "https://adventofcode.com/%d/day/%d/input" year day)
                   {:cookies {"session" {:value @session-cookie}}})))

(def day-input (memoize day-input))

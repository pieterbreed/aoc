(ns aoc.2015.day4
  (:require
   [aoc.common :as -common]
   [instaparse.core :as ip]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math])
  (:import
   [java.security MessageDigest]
   [java.nio.charset StandardCharsets]))

(defn string->hex
  "Converts a string to a hex string using UTF-8 encoding."
  [bs]
  (apply str (map #(format "%02x" %) bs)))

(defn ->md5
  [s]
  (let [d (MessageDigest/getInstance "MD5")
        bs (.getBytes s)
        _ (.update d bs)
        ^bytes digest (.digest d)]
    (string->hex digest)))

(comment

  (->md5 "oeu") ;; "7a2a9bd7deef151cdb5bb04bc0aaccb7"
  (->md5 "abcdef609043") ;; "000001dbbfa3a5c83a2d506429c7b00e"
  (->md5 "pqrstuv1048970") ;; "000006136ef2ff3b291c85725f17325c"


  )

(defn collision?
  [prefix i]
  (str/starts-with? (->md5 (str prefix i)) "00000"))

(defn solve-1
  [input]
  (->> (range)
       (map (fn [i] [i (collision? input i)]))
       (filter second)
       (first)))

(comment
  (solve-1 "abcdef") ;; [609043 true]
  (solve-1 "pqrstuv") ;; [1048970 true]
  (solve-1 "ckczppom") ;; [117946 true]

  )

(defn collision2?
  [prefix i]
  (str/starts-with? (->md5 (str prefix i)) "000000"))

(defn solve-2
  [input]
  (->> (range)
       (map (fn [i] [i (collision2? input i)]))
       (filter second)
       (first)))

(comment

  (solve-2 "ckczppom") ;; [3938038 true]

  )

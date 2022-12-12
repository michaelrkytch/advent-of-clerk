;; # 🎄 Advent of Clerk: Day 0
(ns advent-of-clerk.day-00
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; Read the input file into a string
(def input
  (slurp (io/resource "aoc-input-day1.txt")))

;; Given a \n-delimited sequence of calorie string
;; Parse into a sequence on integers
(defn parse-group [group]
  (let [cal-strs (str/split group #"\n")]
    (map parse-long cal-strs)))

;; Break into sequences of calorie counts, using \n\n as the delimiter
(def calorie-totals
  (let [group-strs (str/split input #"\n\n")
        cal-groups (map parse-group group-strs)]
    (map #(reduce + %) cal-groups))
  )

;; ## Part 1: find the max
(apply max calorie-totals)

;; ## Part 2: sum the top 3
(->> (sort > calorie-totals)
     (take 3)
     (apply +))
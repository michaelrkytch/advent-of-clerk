;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; Read the input file into a string
(def input
  (slurp (io/resource "aoc-input-day4.txt")))

;; Parse the bounds out of the line of the form
;; "2-4,6-8"
;; into a pair of bounds
;; ((2 4)(6 8))
(defn parse-ranges [line]
  (->> line
       (re-seq #"(\d+)-(\d+),(\d+)-(\d+)")
       first
       (drop 1)
       (map #(Integer/parseInt %))
       (partition 2)))

;; Given a pair of endpoints ((2 4)(6 8))
;; Checks if either range fully contains the other
(defn fully-nested? [pairs]
  (let [[p1 p2] (sort-by first pairs)]
    (or
      (and
        (= (first p1) (first p2))
        (>= (last p2) (last p1)))
      (>= (last p1) (last p2)))))

(defn overlap? [pairs]
  (let [[p1 p2] (sort-by first pairs)]
    (or
      (= (first p1) (first p2))
      (>= (last p1) (first p2)))))

;; Part 1
(->> (str/split-lines input)
     (map parse-ranges)
     (map fully-nested?)                                    ;; a sequence of true/false
     (filter identity)                                      ;; keep only the trues
     count)

;; Part 2
(->> (str/split-lines input)
     (map parse-ranges)
     (map overlap?)                                    ;; a sequence of true/false
     (filter identity)                                      ;; keep only the trues
     count)


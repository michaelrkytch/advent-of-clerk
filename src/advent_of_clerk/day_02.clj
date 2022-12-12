;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require
    [clojure.set :as set]
    [nextjournal.clerk :as clerk]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.set :as set]))

(def input
  (slurp (io/resource "aoc-input-day3.txt")))

;; Split string into 2 character sequences of equal size
(defn split-str [s]
  (let [count (count s)
        n (/ count 2)]
    (split-at n s)))

(defn find-dup [groups]
  (let [sets (map set groups)]
    (first (apply set/intersection sets))))

;; Convert ascii code to priority
(defn ascii-to-priority [x]
  (if (> x (int \Z))
    (- x 96 )                                               ;; lower case
    (- x 38)))                                              ;; upper case

;; Part 1
(->> (str/split-lines input)
     (map split-str)
     (map (fn [groups]
            (map #(map int %) groups)))
     (map find-dup)
     (map ascii-to-priority)
     (reduce +))


;; Part 2
(->> (str/split-lines input)
     (partition 3)
     (map (fn [groups]
            (map #(map int %) groups)))
     (map find-dup)
     (map ascii-to-priority)
     (reduce +))
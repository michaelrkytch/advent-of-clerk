;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; How much is each move worth?
(def point-map {:rock 1
                :paper 2
                :scissors 3})

;; Round outcome score for player B is
;; 0 loss
;; 3 draw
;; 6 win
(defn outcome-score [a b]
  (let [score-diff (- (point-map b) (point-map a))]
    (case score-diff
      -2 6                                                  ;; B wins
      -1 0                                                  ;; B loses
      0 3                                                   ;; tie
      1 6                                                   ;; B wins
      2 0                                                   ;; B loses
      )))

(defn round-score [a b]
  (+ (outcome-score a b) (point-map b)))

(defn choose-b [shape-a outcome]
  (case outcome
    :draw shape-a                                                 ;; return same shape to make the round a draw
    :win (case shape-a
           :rock :paper
           :paper :scissors
           :scissors :rock)
    :lose (case shape-a
            :rock :scissors
            :paper :rock
            :scissors :paper)
    ))

(defn decode [a b]
  (let [shape-a  (case a
          "A" :rock
          "B" :paper
          "C" :scissors)
        shape-b (case b
                  "X" :lose
                  "Y" :draw
                  "Z" :win)]
    [shape-a shape-b]))

(def input
  (slurp (io/resource "aoc-input-day2.txt")))

(->> (str/split input #"\n")
     (map #(str/split % #" "))
     (map (fn [[a b]] (decode a b)))
     (map (fn [[a b]] (round-score a (choose-b a b))))
     (reduce +))



;; # 🎄 Advent of Clerk: Day 5
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "aoc-input-day6.txt")))

;; Consume one character of stream
;; Push character onto the end of the buffer
;; Dequeue character if buffer is full
;; Increment position count
;; Return [(rest input), pos, buf]
(defn consume-next [input pos buf len]
  (if (empty? input)
    [nil, pos, buf]
    ;; else consume next char
    (let [nextchar (first input)
          bufplus  (conj buf nextchar)
          ;; dequeue if buf is full
          updated-buf (if (> (count bufplus) len)
                        (vec (rest bufplus))
                        bufplus)]
      [(rest input), (inc pos), updated-buf])))

;; Find the first segment in input that contains len different characters
;; The stream position of the end of this segment.
(defn find-marker [input n]
  (loop [chars input
         pos 0
         buf []
         len n]
    (cond
      ;; this is the termination condition -- we've found a sequence of n distinct chars
      (= len (count buf) (count (distinct buf))) pos
      ;; if we've run out of input, then there's no match
      (empty? chars) nil
      :else       ;; else consume next character and loop
      (let [[remaining-chars pos buf] (consume-next chars pos buf len)]
        (recur remaining-chars pos buf len)))))



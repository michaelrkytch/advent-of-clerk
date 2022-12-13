;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; Read the file, which contains two sections separated by \n\n
;; this produces a sequence of 3 parts
;; the stack diagram,
;; (""),
;; the movement plan
(def file-parts
  (with-open [rdr (clojure.java.io/reader
                    (io/resource "aoc-input-day5.txt"))]
    (doall (partition-by #(= "" %) (line-seq rdr)))))

(def stack-diagram (first file-parts))
(def move-strings (last file-parts))

;; Parse a single move string into a map with keys :n, :from, :to
(defn parse-move [move-str]
  (let [matches (first (re-seq #"move (\d+) from (\d+) to (\d+)" move-str))]
    {:n    (parse-long (matches 1))
     :from (dec (parse-long (matches 2)))
     :to   (dec (parse-long (matches 3)))}))

(defn parse-moves [move-seq]
  (map parse-move move-seq))

;; The input stack diagram looks like
  '("        [F] [Q]         [Q]        "
  "[B]     [Q] [V] [D]     [S]        "
  "[S] [P] [T] [R] [M]     [D]        "
  "[J] [V] [W] [M] [F]     [J]     [J]"
  "[Z] [G] [S] [W] [N] [D] [R]     [T]"
  "[V] [M] [B] [G] [S] [C] [T] [V] [S]"
  "[D] [S] [L] [J] [L] [G] [G] [F] [R]"
  "[G] [Z] [C] [H] [C] [R] [H] [P] [D]"
  " 1   2   3   4   5   6   7   8   9 ")

;; Transform this into a vector of stacks
[[\G \D \V \Z \J \S \B]
 [\Z \S \M \G \V \P]
 [\C \L \B \S \W \T \Q \F]
 [\H \J \G \W \M \R \V \Q]
 [\C \L \S \N \F \M \D]
 [\R \G \C \D]
 [\H \G \T \R \J \D \S \Q]
 [\P \F \V]
 [\D \R \S \T \J]]

(defn parse-stack-diagram [lines]
  (let [last-line (last lines)
        n-stacks (dec (count (str/split last-line #"(\w+)")))
        max-size (dec (count lines))
        ]

    (->> lines
         ;; parse out the "crate labels" from each line
         (map #(map first (partition 1 4 (drop 1 %))))
         ;; transpose the matrix
         (apply map vector)
         ;; reverse the lists so that they become a stack
         (map reverse)
         ;; Trim the stack index off
         (map #(drop 1 %))
         ;; drop the empty slots at the tops of the stacks
         (map (fn [stack] (take-while #(not= \space %) stack)))
         ;; Turn the lists back into stacks (vectors)
         (map vec)
         ;; and turn the whole structure into a vector so we can index each stack
         vec
         )))

(defn top-crates [stacks]
  (->> stacks
       (map peek)
       (apply str)))

;; Part 1 move execution
;; Given a move like
(def move {:n 2, :from 5, :to 2})

;; Perform the specified transformation
;; "move 2 crates from stack 5 to stack 2
;; -> 2 iterations of: pop stack 5, push stack 2
(defn run-move1 [stacks move]
  (let [{n :n
         from :from
         to :to} move]
    (nth
      (iterate
        ;; pop the top value off the "from" stack and push it on the "to" stack
        (fn [stacks]
          (let [val (peek (stacks from))]
            (-> stacks
                (update from pop)
                (update to #(conj % val)))))
        stacks)
      n)                                              ;; do n moves
    ))

;; Part 2 move execution
;; Given a move like
(def move {:n 2, :from 5, :to 2})

;; Perform the specified transformation
;; "move 2 crates from stack 5 to stack 2
;; -> move top 2 from stack 5 to stack 2, maintaining order
(defn run-move2 [stacks move]
  (let [{n :n
         from-idx :from
         to-idx :to} move
        vals (take-last n (stacks from-idx))]                  ;; top n off the "from" stack
    (-> stacks
        (update from-idx  #(vec (drop-last n %)))
        (update to-idx #(apply conj % vals)))))

(defn run-moves [move-fn stacks moves]
  (reduce move-fn stacks moves))

(defn run-simulation [move-fn]
  (let [stacks (parse-stack-diagram stack-diagram)
        moves (parse-moves move-strings)
        end-state (run-moves move-fn stacks moves)]
    (top-crates end-state)))


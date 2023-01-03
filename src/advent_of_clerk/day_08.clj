;; # 🎄 Advent of Clerk: Day 8
(ns advent-of-clerk.day-08
  :require
  [nextjournal.clerk :as clerk]
  [clojure.string :as str]
  [clojure.java.io :as io])

(def input (slurp (io/resource "aoc-input-day9.txt")))

(def sample "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(defn parse-line [line]
  (let [[dir n] (str/split line #" ")]
    [(first dir) (parse-long n)]))

;; Process a line form the input
;; Generates a sequence of [x y] deltas
(defn cmd-to-moves [[dir n]]
  (let [moveDelta {\U [1 0]
                   \D [-1 0]
                   \L [0 -1]
                   \R [0 1]}]
    (repeat n (moveDelta dir))
    ))

;; Map position difference to tail move
(def tail-moves
  {[0 2]   [0 -1]
   [1 2]   [-1 -1]
   [-1 2]  [1 -1]
   [0 -2]  [0 1]
   [-1 -2] [1 1]
   [1 -2]  [-1 1]
   [2 0]   [-1 0]
   [2 1]   [-1 -1]
   [2 2]   [-1 -1]
   [2 -1]  [-1 1]
   [2 -2] [-1 1]
   [-2 0]  [1 0]
   [-2 -1] [1 1]
   [-2 -2] [1 1]
   [-2 1]  [1 -1]
   [-2 2] [1 -1]
   })

;; Given a new head position, where should tail move to
;; Return new tail position
(defn next-tail [head tail]
  (let [delta (map - tail head)]
    (if-let [move (tail-moves delta)]
      (mapv + tail move)
      ;; default: tail does not move
      tail)))

;; For a given move, calculate the next head and tail positions
;; for two adjacent knots (relative head/tail)
(defn next-segment-coords [[head tail] move]
  (let [new-head (vec (map + head move))
        new-tail (vec (next-tail new-head tail))]
    [new-head new-tail]))

;; Part 1

(defn process-input1 [input]
  (let
    ;; Head and tail both start on [0,0]
    [start-pos [[0 0] [0 0]]]
    (->> input
         str/split-lines
         (map parse-line)
         (mapcat cmd-to-moves)
         (reductions next-segment-coords start-pos)
         )))

;; Record the tail position for the next move
;; Accumulate the set of tail positions visited and then return the count
;; of unique tail positions visited.
(defn part1 [input]
  (let [move-seq (process-input1 input)
        tail-pos-set (reduce
                       ;; Add the next tail position to the set of visited positions
                       (fn [visited positions]
                         ;(println "visited: " visited ", tail: " tail)
                         (conj visited (last positions)))
                       #{}
                       move-seq)
        ]
    (count tail-pos-set)
    ))

;; Part 2

(defn next-coords [start-positions move]
  (let [head (first start-positions)
        new-head (mapv + head move)
        rest-pos (rest start-positions)]
    (reductions next-tail new-head rest-pos)
    ))
(defn process-input2 [input]
   (let
     ;; Head and tail both start on [0,0]
     [start-pos (vec (repeat 10 [0 0]))]
     (->> input
          str/split-lines
          (map parse-line)
          (mapcat cmd-to-moves)
          (reductions next-coords start-pos)
          )))

(defn part2 [input]
  (let [move-seq (process-input2 input)
        tail-pos-set (reduce
                       ;; Add the next tail position to the set of visited positions
                       (fn [visited positions]
                         ;(println "visited: " visited ", tail: " tail)
                         (conj visited (last positions)))
                       #{}
                       move-seq)
        ]
    (count tail-pos-set)
    ))


;; TODO refactor: part1 and part2 are identical except for process-input function
;; process-input1 and process-input2 are identical except start-pos and next-coords functions
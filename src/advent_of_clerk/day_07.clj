;; # 🎄 Advent of Clerk: Day 7
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.set :as set]))


(def input (slurp (io/resource "aoc-input-day8.txt")))

(defn parse-input [input]
  (->> input
      str/split-lines
       (map vec)
       (map (fn [coll] (vec (map #(Character/digit % 10) coll))))
       vec))


;; Generate 4 sequences of coordinates, representing scans of the matrix from all 4 directions
;; Apply f to each scan sequence -- e.g. scanning rows left to right,
;; call (f grid (range n)), where n is the number of entries in a row
;; returns a list of 4 row results, one for each scan direction (lr rl tb bt)
(defn visit-trees [f grid]
  (let [nrows (count grid)
        ncols (count (first grid))
        lr (for [y (range nrows)
                 :let [row-set (f grid (for [x (range ncols)]
                                         [x y]))]]
             row-set
             )
        rl (for [y (range nrows)
                 :let [row-set (f grid (for [x (reverse (range ncols))]
                                         [x y]))]]
             row-set
             )
        tb (for [y (range nrows)
                 :let [row-set (f grid (for [x (range ncols)]
                                         [y x]))]]
             row-set
             )
        bt (for [y (range nrows)
                 :let [row-set (f grid (for [x (reverse (range ncols))]
                                         [y x]))]]
             row-set
             )
        ]
    (list lr rl tb bt))
  )

;; Part 1

;; Return the set coordinates for the tallest trees in the given sequence of coordinates
;; grid: the game board
;; coords: sequence of [x y] coordinates for a row
(defn tallest-in-row [grid coords]

  ;; Reducing function takes the state value [visible max]
  ;; visible: set of coordinates of the visible trees
  ;; max height of tallest tree seen so far in the row
  ;; and the sequence value
  ;; x: current index within the row
  (let [rfn (fn [[visible max :as current-state]
                 [x y :as pos]]

              ;; grid is stored as a vector of row vectors, so we index into the
              ;; row (y) and then the column (x)
              (let [curr (get-in grid [y x])]
                (if (> curr max)
                  ;; if current tree is taller than max, mark it visible and update max
                  [(conj visible pos) curr]
                  ;; else, return state unchanged
                  current-state
                  )))
        ;; Initial state -- we have found no tall trees yet, and max height is -1
        initial-state [#{} -1]]
    (first (reduce rfn initial-state coords))
    ))


(->> input
    parse-input
     ;; Get tallest trees for each row from each scan direction
     (visit-trees tallest-in-row)
     ;; Union all the sets of tallest tree coordinates
     (reduce concat)
     (reduce set/union)
     ;; Count total
     count
     )

;; Part 2

;; Return a vector of visibility values for each entry
;; where visibility is the number of continuous entries that is <= the current entry
;; in the coordinate sequence so far
;; grid: the game board
;; coords: sequence of [x y] coordinates for a row
(defn visibilities [grid coords]
  ;; Reducing function takes the state value [scores last-height]
  ;; scores: vector of visibility scores for the sequence so far
  ;; last-height: height of the previous entry in the sequence
  ;; and the sequence value
  ;; x: current index within the row
  (let [rfn (fn [[scores last-height :as current-state]
                 [x y]]
              (let [last-score (last scores)
                    ;; grid is stored as a vector of row vectors, so we index into the
                    ;; row (y) and then the column (x)
                    curr-height (get-in grid [y x])
                    curr-score
                    (if last-score
                      (if (> curr-height last-height)
                        ;; if current tree is taller the previous entry, increment the score
                        (inc last-score)
                        ;; else the current tree is blocked, so it's score resets to 1
                        1
                        )
                      ;; the first entry always has score 0
                      0
                      )]
                [(conj scores curr-score) curr-height]))
        ;; Initial state -- no scores yet, and last height is 0
        initial-state [[] 0]]
    (first (reduce rfn initial-state coords))))


(defn reorder-matrices [[lr rl tb bt]]
  (let [lr (vec lr)
        ;; reverse right-to-left matrix rows
        rl (mapv (comp vec reverse) rl)
        ;; transpose top-to-bottom matrix so that it becomes left-to-right
        tb (apply mapv vector tb)
        ;; reverse then transpose
        bt (->> bt
                (mapv (comp vec reverse))
                (apply mapv vector)
                )]
    [lr rl tb bt])
  )
(->> input
     parse-input
     ;; Get matrix of visibility scores for each scan direction
     (visit-trees visibilities)
     ;; Reorder matrices so that they're all in the same coordinate system
     ;; TODO: this is not great.  Maybe it would have been better to put all the scores into
     ;; a single matrix
     reorder-matrices
     ;; Concat the rows of each matrix into one vector
     (map #(apply concat %))
     ;; multiply 4 matrices in parallel to get the visibility score for each entry
     (apply map *)
     ;; get max
     (apply max)
     )

;; TODO: part 2 is producing 192, which apparently is too small


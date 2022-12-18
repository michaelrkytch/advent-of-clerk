;; # 🎄 Advent of Clerk: Day 6
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]))

(def input (slurp (io/resource "aoc-input-day7.txt")))

(defn create-file [name size]
  {:name name
   :type :file
   :size size})

(defn create-dir [name]
  {:name name
   :type :dir
   :children {}})

(defn map-input-line [line]
  (if-let [cd-match (re-find #"^\$\s*cd\s+(\S+)" line)]
    ;; CD command is either a popd or a cd into a subdir
    (if-let [dir-name (#{".."} (second cd-match))]
      {:type :popd}
      {:type :cd :name (second cd-match)})
    ;; else is it a directory listing?
    (if-let [dir-match (re-find #"^dir\s+(\S+)" line)]
      (create-dir (second dir-match))
      ;; else is it a file listing?
      (if-let [[_ size name] (re-find #"^(\d+)\s+(\S+)" line)]
        (create-file name size)))))

(defn cwd-to-update-keys
  ([cwd]
   (vec (cons :tree (interleave cwd (repeat :children)))))
  ([cwd newkey]
   (conj (cwd-to-update-keys cwd) newkey)))

;; Update the state given one command
;; state consists of
;; { :cwd ["/" "subdir1" "subdir1.1"]
;;   :tree {"/" {:type :dir,
;;               :children { "subdir1" {:type :dir :children {}
;;                           "file-a"  {:type :file :size 1234} }}}
;;
(defn update-state [state command]
  (let [{:keys [cwd tree]} state
        file-name (:name command)
        key-path (cwd-to-update-keys cwd file-name)]
    (case (:type command)
      :dir (assoc-in state key-path command)
      :file (assoc-in state key-path command)
      :cd (update state :cwd #(conj % file-name))
      :popd (update state :cwd pop)
      )))

;; Process input as \n delimited string, returning a filesystem tree
(defn parse-input [input]
  (let [commands (rest (keep map-input-line (str/split-lines input)))
        root-node {:cwd  ["/"]
                   :tree {"/" (create-dir "/")}}]
    (:tree (reduce update-state root-node commands))))

(defn dir-node? [node]
  (and
    (map? node)
    (= :dir (:type node))))

;; Calculate and store the directory sizes
(defn update-sizes [tree]
  (let [f (fn [node]
            ;; If it's a dir without a size, calculate and store the size
            (if (and (dir-node? node)
                     (nil? (:size node)))
              (let [children (:children node)
                    total (->> children
                               vals
                               (keep :size)
                               (map #(if (string? %)
                                       (parse-long %)
                                       %))
                               (reduce +))]
                (assoc node :size total))
              ;; else just return the node unchanged
              node))
        ]
    (walk/postwalk f tree)))

(defn process-input [input]
  (-> input
      parse-input
      update-sizes))

;; Part 1 Sum the sizes of all directories of size < 100000
(defn part1 [tree]
  (->> tree
       (tree-seq map? vals)
       (filter dir-node?)
       (map :size)
       (filter #(< % 100000))
       (reduce +)))

;; Part 2

(def full-tree (process-input input))
(def capacity 70000000)
(def required 30000000)
(def used (- capacity (get-in full-tree ["/" :size])))

(def target (- required used))

(defn part2 [tree]
  (->> tree
       (tree-seq map? vals)
       (filter dir-node?)
       (map :size)
       (sort <)
       (some #(if (> % target) %))))


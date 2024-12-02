(ns aoc2024.day1
  (:require [aoc2024.core :as aoc]))

(defn parse [data]
  (->> (aoc/parse-lines data :ints)
       aoc/transpose                          ; convert 1 list with 2 columns to 2 lists with 1 column each
       (mapv sort)))

(defn part1 [data]
  (reduce + (apply map (comp abs -) (parse data))))

(defn part2 [data]
  (let [[left right] (parse data)
        fs           (frequencies right)]
    (->> left
         (map #((fnil * 0 0) % (fs %)))       ; (fnil * 0 0) returns a fn that calls "*" but replaces nil params with 0
         (reduce +))))

(assert (= 1223326 (part1 (aoc/day 1))))
(assert (= 21070419 (part2 (aoc/day 1))))

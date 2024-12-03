(ns aoc2024.day3
  (:require [aoc2024.core :as aoc]))

(defn part1 [data]
  (->> (re-seq #"mul\((\d+),(\d+)\)" data)
       (map (comp
             (partial apply *)
             (partial map parse-long)
             rest))
       (reduce +)))

(defn part2 [data]
  (->> (re-seq #"(do\(\)|don\'t\(\)|mul\((\d+),(\d+)\))" data)
       (reduce (fn [[active? result] [fn _ p1 p2]]
                 (condp = fn
                   "don't()" [false result]
                   "do()"    [true  result]
                   [active?  (if active?
                               (+ result (* (parse-long p1) (parse-long p2)))
                               result)]))
               [true 0])
       second))

(assert (= 173731097 (part1 (aoc/day 3))))
(assert (= 93729253 (part2 (aoc/day 3))))

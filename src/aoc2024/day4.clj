(ns aoc2024.day4
  (:require [aoc2024.core :as aoc]
            [clojure.string :as str]))

(def grid (->> (aoc/day 4)
               str/split-lines
               aoc/index-2d))

(def coords (keys grid))
(def dirs [-1 0 1])

(def part1-target (vec "XMAS"))
(def part2-target (set [(vec "MAS") (vec "SAM")]))

(defn part1 []
  (count
   (for [di dirs
         dj dirs
         [i j] coords :when (= part1-target
                               (for [n (range 4)]
                                 (get grid [(+ i (* di n))
                                            (+ j (* dj n))] "")))]
     1)))

(defn part2 []
  (count
   (for [[i j] coords :when (and (contains? part2-target
                                            (for [d dirs]
                                              (get grid [(+ i d) (+ j d)] "")))
                                 (contains? part2-target
                                            (for [d dirs]
                                              (get grid [(+ i d) (- j d)] ""))))]
     1)))

(assert (= 2662 (part1)))
(assert (= 2034 (part2)))

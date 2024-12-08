(ns aoc2024.day8
  (:require [aoc2024.core :as aoc]))

(defn part1-points [x0 y0 ux uy len]        ; given a start pos and unit vector, generate a list of possible
  [[(- x0 (* len ux)) (- y0 (* len uy))]])  ; antinodes for part 1 (single item in the list for this part)

(defn part2-points [x0 y0 ux uy len]
  (for [t (range -100 100)                  ; for part 2, draw a line that extends arbitrarily far off the
        :let [x (int (+ x0 (* len t ux)))   ; grid in both directions. it will be pruned to in-bounds later.
              y (int (+ y0 (* len t uy)))]]
    [x y]))

(defn antinode [points-fn [x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len (int (Math/sqrt (+ (* dx dx) (* dy dy))))
        ux (/ dx len)
        uy (/ dy len)                                  ; pair of points => unit vector
        points (points-fn x1 y1 ux uy len)]            ; unit vector => possible antinotes
    points))

(defn all-antinodes [points-fn [_freq antennas]]
  (when-let [pairs (for [a1 antennas a2 antennas :when (not= a1 a2)] [a1 a2])]
    (mapcat (partial apply antinode points-fn) pairs)))

(defn in-bounds [width height [x y]]
  (and (<= 0 x (dec width)) (<= 0 y (dec height))))

(defn solve [data points-fn]
  (let [lines (aoc/parse-lines data)
        width (count (first lines))
        height (count lines)]
    (->> (aoc/index-2d lines)                       ; build map of [x y] => char
         (filter (fn [[_ v]] (not= \. v)))          ; remove empty spaces
         (group-by second)                          ; invert to map of char => [[[x1 y1] char] [[x2 y2] char]]
         (map (fn [[k vs]] [k (map first vs)]))     ; filter out redundant chars, so map is char => [[x1 y1] [x2 y2]]
         (mapcat (partial all-antinodes points-fn)) ; convert to list of antinodes
         (filter (partial in-bounds width height))  ; remove out of bounds
         (into #{})                                 ; de-dupe with a set
         count)))

(assert (= 357  (solve (aoc/day 8) part1-points)))
(assert (= 1266 (solve (aoc/day 8) part2-points)))

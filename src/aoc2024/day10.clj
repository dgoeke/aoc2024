(ns aoc2024.day10
  (:require [aoc2024.core :as aoc]))

(def dirs [[0 1] [0 -1] [1 0] [-1 0]])

(defn explore [terr pos keep?]
  (let [h (terr pos)
        terr (if-not keep? (dissoc terr pos) terr)]
    (if (= h 9)
      1
      (->> dirs
           (map (fn [[dr dc]] [(+ (pos 0) dr) (+ (pos 1) dc)]))
           (keep #(when (= (terr %) (inc h))  ; Check if next height is current + 1
                    (explore terr % keep?)))
           (reduce + 0)))))

(let [terrain (->> (aoc/parse-lines (aoc/day 10) :digits)
                   (aoc/index-2d))
      starts (keep (fn [[k v]] (when (zero? v) k)) terrain)
      [part1 part2] (map (fn [keep?] (reduce + (map #(explore terrain % keep?) starts)))
                         [true false])]
  (assert (= 646 part1))
  (assert (= 1494 part2)))

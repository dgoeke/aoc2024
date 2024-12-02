(ns aoc2024.day2
  (:require [aoc2024.core :as aoc]))

(defn parse [data] (aoc/parse-lines data :ints))

(defn differences [line]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 line)))     ; [1 5 3 2] => [4 -2 -1]

(defn safe? [deltas]
  (and (or (every? pos? deltas) (every? neg? deltas))   ; all ascending or all descendeing
       (every? #(<= 1 % 3) (map abs deltas))))          ; ... by either 1, 2, or 3

(defn part1 [data]
  (count (filter safe? (->> data parse (map differences)))))

(defn remove-nth [v n]
  (vec (concat (subvec v 0 n) (subvec v (inc n)))))

(defn safe-with-one-removed? [data]
  (some safe? (into (mapv #(differences (remove-nth data %)) (range (count data)))
                    [(differences data)])))

(defn part2 [data]
  (count (filter safe-with-one-removed? (parse data))))

(assert (= 334 (part1 (aoc/day 2))))
(assert (= 400 (part2 (aoc/day 2))))

(ns aoc2024.day6
  (:require [aoc2024.core :as aoc]
            [clojure.string :as str]))

(def dx [0 1 0 -1])
(def dy [-1 0 1 0])

(defn find-start [grid]
  (first (for [y (range (count grid))
               x (range (count (first grid)))
               :when (= \^ (get-in grid [y x]))]
           [y x])))

(defn make-moves [grid start pos dir obstacle seen]
  (let [h (count grid)
        w (count (first grid))
        next-y (+ (first pos) (dy dir))
        next-x (+ (second pos) (dx dir))
        next-pos [next-y next-x]]
    (cond
      ;; Out of bounds
      (or (< next-y 0) (>= next-y h)
          (< next-x 0) (>= next-x w))
      false

      ;; Hit wall or obstacle
      (or (= \# (get-in grid [next-y next-x]))
          (= next-pos obstacle))
      (let [new-dir (mod (inc dir) 4)
            state [next-pos new-dir]]
        (if (contains? seen state)
          true
          (recur grid start pos new-dir obstacle (conj seen state))))

      ;; Regular move
      :else
      (let [state [next-pos dir]]
        (if (contains? seen state)
          true
          (recur grid start next-pos dir obstacle (conj seen state)))))))

(defn patrol [grid obstacle]
  (let [start (find-start grid)]
    (make-moves grid start start 0 obstacle #{[start 0]})))

(defn part1 [input]
  (let [grid (str/split-lines input)
        h (count grid)
        w (count (first grid))
        start (find-start grid)]
    (loop [pos start
           dir 0
           seen #{start}]
      (let [next-y (+ (first pos) (dy dir))
            next-x (+ (second pos) (dx dir))
            next-pos [next-y next-x]]
        (cond
          (or (< next-y 0) (>= next-y h)
              (< next-x 0) (>= next-x w))
          (count seen)

          (= \# (get-in grid [next-y next-x]))
          (recur pos (mod (inc dir) 4) seen)

          :else
          (recur next-pos dir (conj seen next-pos)))))))

(defn part2 [input]
  (let [grid (str/split-lines input)
        h (count grid)
        w (count (first grid))
        start (find-start grid)]
    (->> (for [y (range h)
               x (range w)
               :when (and (= \. (get-in grid [y x]))
                         (not= [y x] start))]
           [y x])
         (filter #(patrol grid %))
         count)))

(assert (= 5305 (part1 (aoc/day 6))))
(assert (= 2143 (part2 (aoc/day 6))))

(ns aoc2024.day7
  (:require [aoc2024.core :as aoc]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.string :as str]))

(defn || [& args] (->> (map str args) str/join parse-long))

;; calculate all operator sequences of a given length, cache the result
(def operator-seqs
  (memoize (fn [operators n]
             (vec (for [x (apply cartesian-product (repeat n operators))]
                    (vec x))))))

;; (calculate [+ *] [1 2 3]) computes (1+2)*3 and returns 9
(defn calculate [operators vals]
  (first (reduce (fn [[result [op & ops]] val]
                   [(op result val) ops])
                 [(first vals) operators] (rest vals))))

;; check a single line of input with a list of available operators
(defn valid-formula? [operators [result & vals]]
  (some #(when (= result (calculate % vals)) result)
        (operator-seqs operators (dec (count vals)))))

(defn solve [operators input]
  (->> (aoc/parse-lines input :ints)
       (keep (partial valid-formula? operators))
       (reduce +)))

(assert (= 1038838357795   (solve [* +]    (aoc/day 7))))
(assert (= 254136560217241 (solve [* + ||] (aoc/day 7))))

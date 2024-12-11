(ns aoc2024.day11
  (:require [aoc2024.core :as aoc]
            [clojure.string :as str]))

(defn transform [counts]
  (reduce-kv
   (fn [new-counts s cnt]
     (if (= s "0")
       (update new-counts "1" (fnil + 0) cnt)
       (let [digits (count s)]
         (if (even? digits)
           (let [half (quot digits 2)
                 left-part (str (parse-long (subs s 0 half)))
                 right-part (str (parse-long (subs s half)))]
             (-> new-counts
                 (update left-part (fnil + 0) cnt)
                 (update right-part (fnil + 0) cnt)))
           (let [val (* (Integer/parseInt s) 2024)]
             (update new-counts (str val) (fnil + 0) cnt))))))
   {}
   counts))

(defn solve []
  (let [stones  (aoc/parse-line (str/trim (aoc/day 11)) :words)
        initial-counts (reduce (fn [counts stone]
                                 (update counts stone (fnil inc 0)))
                               {}
                               stones)
        result (vec (->> (take 76 (iterate transform initial-counts))
                         (map #(reduce + (vals %)))))]
    [(get result 25) (get result 75)]))

(let [[part1 part2] (solve)]
  (assert (= 212655 part1))
  (assert (= 253582809724830 part2)))

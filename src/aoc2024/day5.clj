(ns aoc2024.day5
  (:require [aoc2024.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-rules [rules-str]
  (->> rules-str
       str/split-lines
       (keep #(when-not (str/blank? %)
               (let [[before after] (mapv parse-long (str/split % #"\|"))]
                 [before #{after}])))
       (reduce #(update %1 (first %2) (fnil set/union #{}) (second %2)) {})))

(defn parse-sequences [seq-str]
  (->> seq-str
       str/split-lines
       (keep #(when-not (str/blank? %)
               (mapv parse-long (str/split % #","))))
       vec))

(defn parse-input [input]
  (let [[rules seqs] (str/split input #"\n\n")]
    [(parse-rules rules) (parse-sequences seqs)]))

(defn valid-sequence? [seq rules]
  (let [seq-set (set seq)]
    (->> (range (count seq))
         (not-any? (fn [i]
                     (when-let [matches (get rules (nth seq i))]
                       (let [remaining (set (drop (inc i) seq))]
                         (some #(and (contains? seq-set %)
                                     (not (contains? remaining %)))
                               matches))))))))

(defn find-next-valid [seq used rules]
  (let [seq-set (set seq)]
    (->> seq
         (some #(when-not (contains? used %)
                  (when (every? (fn [[b after]]
                                  (or (not (contains? seq-set b))
                                      (contains? used b)
                                      (not (contains? after %))))
                                rules)
                    %))))))

(defn sort-sequence [seq rules]
  (reduce
    (fn [acc _]
      (when-let [next-elem (find-next-valid seq (set acc) rules)]
        (conj acc next-elem)))
    []
    (range (count seq))))

(def middle-element (comp #(nth % (quot (count %) 2)) vec))

(defn part1 [input]
  (let [[rules sequences] (parse-input input)]
    (->> sequences
         (filter #(valid-sequence? % rules))
         (transduce (map middle-element) + 0))))

(defn part2 [input]
  (let [[rules sequences] (parse-input input)]
    (->> sequences
         (remove #(valid-sequence? % rules))
         (keep #(sort-sequence % rules))
         (transduce (map middle-element) + 0))))

(assert (= 5651 (part1 (aoc/day 5))))
(assert (= 4743 (part2 (aoc/day 5))))

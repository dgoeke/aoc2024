(ns aoc2024.day9
  (:require [aoc2024.core :as aoc]))

;; look I don't love this whole solution either but it works so we're moving on

(defn make-disk-state [digits]
  (loop [remaining digits
         disk [] file-blocks [] free-blocks [] file-id 0 pos 0]
    (if (empty? remaining)
      {:disk disk :file-blocks file-blocks :free-blocks free-blocks}
      (let [block-len (first remaining)]
        (if (even? pos)
          ; File block
          (recur (rest remaining)
                 (into disk (repeat block-len file-id))
                 (conj file-blocks [(.size disk) block-len])
                 free-blocks
                 (inc file-id)
                 (inc pos))
          ; Free block
          (recur (rest remaining)
                 (into disk (repeat block-len "."))
                 file-blocks
                 (conj free-blocks [(.size disk) block-len])
                 file-id
                 (inc pos)))))))

(defn file-positions [file-blocks]
  (mapcat (fn [[start length]] (range (+ start (dec length)) (dec start) -1))
          (reverse file-blocks)))

(defn part1 [disk file-blocks]
  (loop [idx-file (first (file-positions file-blocks))
         remaining-positions (rest (file-positions file-blocks))
         idx-disk 0 checksum 0]
    (cond
      (> idx-disk idx-file) checksum
      (= (nth disk idx-disk) ".") (recur (first remaining-positions)
                                         (rest remaining-positions)
                                         (inc idx-disk)
                                         (+ checksum (* idx-disk (nth disk idx-file))))
      :else (recur idx-file remaining-positions (inc idx-disk)
                   (+ checksum (* idx-disk (nth disk idx-disk)))))))

(defn update-disk-segment [disk start len val]
  (vec (concat (subvec disk 0 start)
               (repeat len val)
               (subvec disk (+ start len)))))

(defn copy-disk-segment [disk src-start dest-start len]
  (let [segment (subvec disk src-start (+ src-start len))]
    (vec (concat (subvec disk 0 dest-start)
                 segment
                 (subvec disk (+ dest-start len))))))

(defn part2 [disk file-blocks free-blocks]
  (loop [curr-disk disk
         remaining-files (reverse file-blocks)
         curr-free-blocks (vec free-blocks)]
    (if (empty? remaining-files)
      (reduce + (map-indexed #(if (not= %2 ".") (* %1 %2) 0) curr-disk))
      (let [[file-start file-len] (first remaining-files)
            free-block-match (first
                              (filter (fn [[free-start free-len]]
                                        (and (< free-start file-start) (>= free-len file-len)))
                                      curr-free-blocks))
            free-block-idx (.indexOf curr-free-blocks free-block-match)]
        (if free-block-match
          (let [[free-start free-len] free-block-match]
            (recur (-> curr-disk
                       (copy-disk-segment file-start free-start file-len)
                       (update-disk-segment file-start file-len "."))
                   (rest remaining-files)
                   (if (< file-len free-len)
                     (assoc curr-free-blocks free-block-idx
                            [(+ free-start file-len) (- free-len file-len)])
                     (vec (concat (subvec curr-free-blocks 0 free-block-idx)
                                  (subvec curr-free-blocks (inc free-block-idx)))))))
          (recur curr-disk (rest remaining-files) curr-free-blocks))))))

(let [digits (seq (aoc/parse-line (aoc/day 9) :digits))
      {:keys [disk file-blocks free-blocks]} (make-disk-state digits)]
  (assert (= 6461289671426 (part1 disk file-blocks)))
  (assert (= 6488291456470 (part2 disk file-blocks free-blocks))))

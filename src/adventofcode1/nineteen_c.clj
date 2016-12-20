(ns adventofcode1.nineteen-c
  "http://adventofcode.com/2016/day/19"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tcgen]
   [clojure.test.check.properties :as tcprop]

   [clojure.core.match :refer [match]]

   [clojure.core.rrb-vector :as fv]
   mw.utils
   spyscope.core
   adventofcode1.spec-test-instrument-debug))


;; (require '[clojure.core.rrb-vector :as fv])
;; (let [s (vec [0 1 2 3 4])]   (fv/catvec (fv/subvec s 0 2) (fv/subvec s 3 5)))
;; ; => [0 1 3 4]

(defn catvec
  [a b]
  (vec (concat a b)))

(defn remove-at
  "Remove cell at idx in arr."
  [arr idx]
  (catvec (subvec arr 0 idx) (subvec arr (inc idx))))


(defn create-arr
  "Return a vector with pair [1 idx] where idx starts at 1...size (incl)."
  [size]
  (vec (for [x (range 1 (inc size))]
            [1 x])))

(defn fv-rest
  [arr]
  (subvec arr 1))

(defn calculate-opposite
  "n is the number of elfs incl me. Im a at pos 0.
   Return the opposite position."
  [n]
  (int (/ n 2)))


(defn move
  [elfs]
  (let [lc (count elfs)]
    (if (= 1 lc)
      {:ok (first elfs)}
      (let [current      (first elfs)
            opposite-pos (calculate-opposite lc)
            _ (assert (> opposite-pos 0))
            _ (assert (< opposite-pos lc))
            opposite-elf (nth elfs opposite-pos)
            other2       (fv-rest (remove-at elfs opposite-pos))
            current2     [(+ (first current)(first opposite-elf))(second current)]]
        (catvec other2 [current2])))))


(defn puzzle-b-sample
  ([] (puzzle-b-sample (create-arr 5)))
  ([elfs] (let [elfs2 (move elfs)]
            (if (:ok elfs2)
              (:ok elfs2)
              ;;(println elfs2)
              (recur elfs2)))))

(s/fdef puzzle-b
        :args (s/cat :n (s/and int? pos?))
        :ret  (s/coll-of int?))

(defn puzzle-b
  ;; ([](puzzle-b 3014603))
  ([](puzzle-b 978))
  ([n](puzzle-b-sample (create-arr n))))

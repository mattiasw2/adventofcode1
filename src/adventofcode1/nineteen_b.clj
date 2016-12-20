(ns adventofcode1.nineteen-b
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

(defn remove-at
  "Remove cell at idx in arr."
  [arr idx]
  (fv/catvec (fv/subvec arr 0 idx) (fv/subvec arr (inc idx))))


(defn create-arr
  "Return a vector with pair [1 idx] where idx starts at 1...size (incl)."
  [size]
  (fv/vec (for [x (range 1 (inc size))]
            [1 x])))

(defn fv-rest
  [arr]
  (fv/subvec arr 1))


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
        (fv/catvec other2 [current2])))))


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
  ([](puzzle-b 3014603))
  ([n](puzzle-b-sample (create-arr n))))

;; By running (stest/check `puzzle-b)
;; the smallest case I find that that fails with
;; ArrayIndexOutOfBoundsException 33  clojure.core.rrb-vector.rrbt.Vector/fn--19277 (rrbt.clj:373)
;; is (puzzle-b 978)
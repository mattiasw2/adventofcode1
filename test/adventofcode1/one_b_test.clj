(ns adventofcode1.one-b-test
  (:require
   ;; [schema.core :as s]
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.one-b :refer :all]
   )
  )


;; Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
;; R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
;; R5, L5, R5, R3 leaves you 12 blocks away.
(deftest total-distance-test
  (is (= 5 (total-distance [{:r 2}{:l 3}])))
  (is (= 2 (total-distance [{:r 2}{:r 2}{:r 2}])))
  (is (= 0 (total-distance [{:r 2}{:r 0}{:r 2}])))
  (is (= 2 (total-distance [{:r 2}{:r 0}{:r 2}{:r 2}])))
  (is (= 12 (total-distance [{:r 5}{:l 5}{:r 5}{:r 3}])))
  )


;; R2, L3, R2, R4, L2, L1, R2, R4, R1, L4, L5, R5, R5, R2, R2, R1, L2, L3, L2, L1, R3, L5, R187, R1, R4, L1, R5, L3, L4, R50, L4, R2, R70, L3, L2, R4, R3, R194, L3, L4, L4, L3, L4, R4, R5, L1, L5, L4, R1, L2, R4, L5, L3, R4, L5, L5, R5, R3, R5, L2, L4, R4, L1, R3, R1, L1, L2, R2, R2, L3, R3, R2, R5, R2, R5, L3, R2, L5, R1, R2, R2, L4, L5, L1, L4, R4, R3, R1, R2, L1, L2, R4, R5, L2, R3, L4, L5, L5, L4, R4, L2, R1, R1, L2, L3, L2, R2, L4, R3, R2, L1, L3, L2, L4, L4, R2, L3, L3, R2, L4, L3, R4, R3, L2, L1, L4, R4, R2, L4, L4, L5, L1, R2, L5, L2, L3, R2, L2

(def puzzle-1
  [{:r 2}, {:l 3}, {:r 2}, {:r 4}, {:l 2}, {:l 1}, {:r 2}, {:r 4}, {:r 1}, {:l 4}, {:l 5}, {:r 5}, {:r 5}, {:r 2}, {:r 2}, {:r 1}, {:l 2}, {:l 3}, {:l 2}, {:l 1}, {:r 3}, {:l 5}, {:r 187}, {:r 1}, {:r 4}, {:l 1}, {:r 5}, {:l 3}, {:l 4}, {:r 50}, {:l 4}, {:r 2}, {:r 70}, {:l 3}, {:l 2}, {:r 4}, {:r 3}, {:r 194}, {:l 3}, {:l 4}, {:l 4}, {:l 3}, {:l 4}, {:r 4}, {:r 5}, {:l 1}, {:l 5}, {:l 4}, {:r 1}, {:l 2}, {:r 4}, {:l 5}, {:l 3}, {:r 4}, {:l 5}, {:l 5}, {:r 5}, {:r 3}, {:r 5}, {:l 2}, {:l 4}, {:r 4}, {:l 1}, {:r 3}, {:r 1}, {:l 1}, {:l 2}, {:r 2}, {:r 2}, {:l 3}, {:r 3}, {:r 2}, {:r 5}, {:r 2}, {:r 5}, {:l 3}, {:r 2}, {:l 5}, {:r 1}, {:r 2}, {:r 2}, {:l 4}, {:l 5}, {:l 1}, {:l 4}, {:r 4}, {:r 3}, {:r 1}, {:r 2}, {:l 1}, {:l 2}, {:r 4}, {:r 5}, {:l 2}, {:r 3}, {:l 4}, {:l 5}, {:l 5}, {:l 4}, {:r 4}, {:l 2}, {:r 1}, {:r 1}, {:l 2}, {:l 3}, {:l 2}, {:r 2}, {:l 4}, {:r 3}, {:r 2}, {:l 1}, {:l 3}, {:l 2}, {:l 4}, {:l 4}, {:r 2}, {:l 3}, {:l 3}, {:r 2}, {:l 4}, {:l 3}, {:r 4}, {:r 3}, {:l 2}, {:l 1}, {:l 4}, {:r 4}, {:r 2}, {:l 4}, {:l 4}, {:l 5}, {:l 1}, {:r 2}, {:l 5}, {:l 2}, {:l 3}, {:r 2}, {:l 2}])

;; 246 was the correct answer
(deftest puzzle-1-test
  (is (= 246 (total-distance adventofcode1.one-test/puzzle-1))))


(deftest stop-same-position-test
  (is (= "No easter bunny found" (stop-same-position [{:r 2}{:l 3}])))
  (is (= "No easter bunny found" (stop-same-position [{:r 2}{:r 2}{:r 2}])))
  (is (= "No easter bunny found" (stop-same-position [{:r 5}{:l 5}{:r 5}{:r 3}])))
  ;; bad tests, since zero steps means same position, so abort
  (is (= 2 (stop-same-position [{:r 2}{:r 0}{:r 2}])))
  (is (= 2 (stop-same-position [{:r 2}{:r 0}{:r 2}{:r 2}])))
  ;; more realistic sample
  (is (= 0 (stop-same-position [{:r 2}{:r 2}{:r 2}{:r 2}])))
  (is (= 0 (stop-same-position [{:r 2}{:r 2}{:r 2}{:r 2}{:r 5}])))
  ;; and their sample
  (is (= 4 (stop-same-position [{:r 8}{:r 4}{:r 4}{:r 8}])))
  )

(deftest puzzle-1b-test
  (is (= 124 (stop-same-position adventofcode1.one-test/puzzle-1))))


;; todo: core-path can be tested by making sure same result
;; (total-distance (core-path [{:l 2} {:r 1} {:l 7} {:l 0}]))
;; 10
;; (total-distance [{:l 2} {:r 1} {:l 7} {:l 0}])
;; 10

(ns adventofcode1.clpfd10b-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]
   [clojure.core.logic :refer :all  :exclude [is]]

   spyscope.core
   [adventofcode1.clpfd10b :refer :all]))

(deftest goal-wolf-cabbage-goat-test
  (is (= '([0 0 0 0])
         (run 1 [s1] (is_state s1)(start_state s1))))
  (is (= '([1 1 1 1])
         (run 1 [s1] (is_state s1)(end_state s1)))))




;; made a change

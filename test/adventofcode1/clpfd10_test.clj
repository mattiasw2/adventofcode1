(ns adventofcode1.clpfd10-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.clpfd10 :refer :all]))

(deftest goal-wolf-cabbage-goat-test
  (is (= '({:from 0, :who :goat, :to 1} {:from 1, :who :nothing, :to 0} {:from 0, :who :wolf, :to 1} {:from 1, :who :goat, :to 0} {:from 0, :who :cabbage, :to 1} {:from 1, :who :nothing, :to 0} {:from 0, :who :goat, :to 1})
         (first (goal 7))))
  (is (= '({:from 0, :who :goat, :to 1} {:from 1, :who :nothing, :to 0} {:from 0, :who :wolf, :to 1} {:from 1, :who :goat, :to 0} {:from 0, :who :cabbage, :to 1} {:from 1, :who :nothing, :to 0} {:from 0, :who :goat, :to 1})
         (first (goal_unq 7)))))

;; made a change

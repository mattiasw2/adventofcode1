(ns adventofcode1.clpfd10-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.clpfd10 :refer :all]))

(deftest goal-wolf-cabbage-goat-test
  (is (= '([0 :goat 1] [1 :nothing 0] [0 :wolf 1] [1 :goat 0] [0 :cabbage 1] [1 :nothing 0] [0 :goat 1])
         (first (goal 7))))
  (is (= '([0 :goat 1] [1 :nothing 0] [0 :wolf 1] [1 :goat 0] [0 :cabbage 1] [1 :nothing 0] [0 :goat 1])
         (first (goal_unq 7)))))

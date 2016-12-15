(ns adventofcode1.clpfd10-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.clpfd10 :refer :all]))

(deftest goal-wolf-cabbage-goat-test
  (is (= '(:goat :nothing :wolf :goat :cabbage :nothing :goat)
         (first (goal 7))))
  (is (= '(:goat :nothing :wolf :goat :cabbage :nothing :goat)
         (first (goal_unq 7)))))

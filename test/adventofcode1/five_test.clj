(ns adventofcode1.five-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.five :refer :all]))


(deftest next-chars-tmp-test
  (is (= '(3231929 5017308 5278568)
         (take 3 (adventofcode1.five/next-chars-temp "abc" 3231900)))))

(deftest next-chars-1-test
  (is (= ["1"]
         (take 1 (adventofcode1.five/next-chars "abc" 3231900)))))

(deftest next-chars-2-test
  (is (= ["1" "8" "f"]
         (take 3 (adventofcode1.five/next-chars "abc" 3231900)))))

(deftest puzzle-5-test
  (is (= "f77a0e6e"
         (puzzle-five "cxdnnyjw"))))

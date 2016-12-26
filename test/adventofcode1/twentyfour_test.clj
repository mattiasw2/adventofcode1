(ns adventofcode1.twentyfour-test
  "http://adventofcode.com/2016/day/24"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tcgen]
   [clojure.test.check.properties :as tcprop]

   [clojure.core.match :refer [match]]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [com.rpl.specter :as sp]

   spyscope.core
   mw.utils
   adventofcode1.spec-test-instrument-debug

   [adventofcode1.twentyfour :refer :all]
   [clojure.test :refer :all]))


(def board-0
  (board-make
   "####
    #..#
    ####"))

(def board-2
  (board-make
   "######
    #..#.#
    ######"))

(def board-3
  (board-make
   "######
    #...##
    ######"))

(def board-4
 (board-make
  "###########
  #0.1.....2#
  #.###.###.#
  #4.......3#
  ###########"))


(deftest horizontal-paths-tests
  (is (= [ {:from [1 1], :to [1 2]} ] (horizontal-paths board-0 1)))
  (is (= [ {:from [1 1], :to [1 2]} ] (horizontal-paths board-2 1)))
  (is (= [ {:from [1 1], :to [1 3]} ] (horizontal-paths board-3 1)))
  (is (= [ {:from [1 1], :to [1 9]} ] (horizontal-paths board-4 1))))

(deftest vertical-paths-tests
  (is (= [ {:from [1 1], :to [3 1]} ] (vertical-paths board-4 1)))
  (is (= [  ]                         (vertical-paths board-4 3)))
  (is (= [ {:from [1 5], :to [3 5]} ] (vertical-paths board-4 5)))
  (is (= [ {:from [1 9], :to [3 9]} ] (vertical-paths board-4 9))))

(deftest split-horizontal-path-at-test
  ;; path: ({:from [1 1], :to [1 9]} {:from [3 1], :to [3 9]})
  (let [path (horizontal-paths board-4)]
    ;; first row
    (is (= '((101 1 1) (101 1 3) (3 1 5) (101 1 9))
           (split-horizontal-path-at board-4 (first path))))
    ;; 3rd row, since no path on 2nd row and exactly one path on first
    (is (= '((101 3 1) (3 3 5) (101 3 9))
           (split-horizontal-path-at board-4 (second path))))))

(deftest split-vertical-path-at-test
  ;; path: ({:from [1 1], :to [3 1]} {:from [1 5], :to [3 5]} {:from [1 9], :to [3 9]})
  (let [path (vertical-paths board-4)
        _ (println path)]
    (is (= '((101 1 1) (101 3 1))
           (split-vertical-path-at board-4 (first path))))))

(deftest split-horizontal-path-at-test
  (is (= '({:from [1 1], :to [1 3]})
         (split-horizontal-path {:from [1 1], :to [1 3]} '((101 1 1) (101 1 3)))))
  (is (= '({:from [1 2], :to [1 3]} {:from [1 1], :to [1 2]})
         (split-horizontal-path {:from [1 1], :to [1 3]} '((101 1 1) (101 1 2)))))
  (is (= '({:from [1 2], :to [1 3]} {:from [1 1], :to [1 2]})
         (split-horizontal-path {:from [1 1], :to [1 3]} '((101 1 2) (101 1 3))))))


(deftest split-vertical-path-at-test
  (is (= '({:from [1 1], :to [3 1]})
         (split-vertical-path {:from [1 1], :to [3 1]} '((101 1 1) (101 3 1)))))
  (is (= '({:from [2 1], :to [3 1]} {:from [1 1], :to [2 1]})
         (split-vertical-path {:from [1 1], :to [3 1]} '((101 1 1) (101 2 1)))))
  (is (= '({:from [2 1], :to [3 1]} {:from [1 1], :to [2 1]})
         (split-vertical-path {:from [1 1], :to [3 1]} '((101 2 1) (101 3 1))))))

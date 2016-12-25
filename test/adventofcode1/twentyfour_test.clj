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
  (is (= [ {:from [1 1], :to [1 3]} ] (vertical-paths board-4 1)))
  (is (= [  ]                         (vertical-paths board-4 3)))
  (is (= [ {:from [5 1], :to [5 3]} ] (vertical-paths board-4 5)))
  (is (= [ {:from [9 1], :to [9 3]} ] (vertical-paths board-4 9))))

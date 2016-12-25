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


(deftest horizontal-paths-tests
  (is (= [ {:from [1 1], :to [1 2]} ] (horizontal-paths board-0 1)))
  (is (= [ {:from [1 1], :to [1 2]} ] (horizontal-paths board-2 1)))
  (is (= [ {:from [1 1], :to [1 3]} ] (horizontal-paths board-3 1))))

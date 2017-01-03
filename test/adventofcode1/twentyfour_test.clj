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


(deftest numbered-cells-test
  (is (= '({:digit \2, :to [3 149]} {:digit \1, :to [5 7]} {:digit \0, :to [11 149]} {:digit \7, :to [17 171]} {:digit \3, :to [17 3]} {:digit \5, :to [27 143]} {:digit \6, :to [27 27]} {:digit \4, :to [35 137]})
         (numbered-cells (board-make puzzle-input-a))))
  (is (= '({:digit \2, :to [1 9]} {:digit \1, :to [1 3]} {:digit \0, :to [1 1]} {:digit \3, :to [3 9]} {:digit \4, :to [3 1]})
         (numbered-cells board-4)))
  (is (= '()
         (numbered-cells board-3))))

(deftest digit-pos-map-test
  (let [t (numbered-cells adventofcode1.twentyfour-test/board-4 1)]
    (is (= {\2 [1 9], \1 [1 3], \0 [1 1]}
           (digit-pos-map t)))
    (is (= {[1 9] \2, [1 3] \1, [1 1] \0}
           (pos-digit-map t)))))



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

(deftest split-horizontal-path-test
  (is (= '({:from [1 1], :to [1 3]})
         (split-horizontal-path {:from [1 1], :to [1 3]} '((101 1 1) (101 1 3)))))
  (is (= '({:from [1 2], :to [1 3]} {:from [1 1], :to [1 2]})
         (split-horizontal-path {:from [1 1], :to [1 3]} '((101 1 1) (101 1 2)))))
  (is (= '({:from [1 2], :to [1 3]} {:from [1 1], :to [1 2]})
         (split-horizontal-path {:from [1 1], :to [1 3]} '((101 1 2) (101 1 3))))))


(deftest split-vertical-path-test
  (is (= '({:from [1 1], :to [3 1]})
         (split-vertical-path {:from [1 1], :to [3 1]} '((101 1 1) (101 3 1)))))
  (is (= '({:from [2 1], :to [3 1]} {:from [1 1], :to [2 1]})
         (split-vertical-path {:from [1 1], :to [3 1]} '((101 1 1) (101 2 1)))))
  (is (= '({:from [2 1], :to [3 1]} {:from [1 1], :to [2 1]})
         (split-vertical-path {:from [1 1], :to [3 1]} '((101 2 1) (101 3 1)))))
  (is (= '{[1 1] [{:from [1 1], :to [3 1]}],
           [1 9] [{:from [1 9], :to [3 9]}],
           [3 1] [{:from [1 1], :to [3 1]}],
           [3 9] [{:from [1 9], :to [3 9]}]}
         (vertical-splitted-paths {} (board-make sample1)))))


(deftest all-paths-sample1-test
  (is (=
       {[1 1] [{:from [1 1], :to [3 1]} {:from [1 1], :to [1 3]}]
        [1 9] [{:from [1 9], :to [3 9]} {:from [1 3], :to [1 9]}]
        [3 1] [{:from [1 1], :to [3 1]} {:from [3 1], :to [3 9]}]
        [3 9] [{:from [1 9], :to [3 9]} {:from [3 1], :to [3 9]}]
        [1 3] [{:from [1 3], :to [1 9]} {:from [1 1], :to [1 3]}]}
       (all-paths (board-make sample1)))))


(deftest all-paths-sample2-test
  (is (=
       {[1 1] [{:from [1 1], :to [3 1]} {:from [1 1], :to [1 3]}]
        , [1 5] [{:from [1 5], :to [3 5]} {:from [1 5], :to [1 9]} {:from [1 3], :to [1 5]}]
        , [1 9] [{:from [1 9], :to [3 9]} {:from [1 5], :to [1 9]}]
        , [3 1] [{:from [1 1], :to [3 1]} {:from [3 1], :to [3 5]}]
        , [3 5] [{:from [1 5], :to [3 5]} {:from [3 5], :to [3 6]} {:from [3 1], :to [3 5]}]
        , [3 9] [{:from [1 9], :to [3 9]} {:from [3 6], :to [3 9]}]
        , [1 3] [{:from [1 3], :to [1 5]} {:from [1 1], :to [1 3]}]
        , [3 6] [{:from [3 6], :to [3 9]} {:from [3 5], :to [3 6]}]}
       (all-paths (board-make sample2)))))


(deftest assoc-duplicates-test
  (is (=
       {:a [11]}
       (assoc-duplicates {} [:a 11])))
  (is (=
       {:a [19], :b [20 21]}
       (into-assoc-duplicates {} [[:a 19][:b 20][:b 21]]))))

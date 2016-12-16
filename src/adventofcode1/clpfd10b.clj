(ns adventofcode1.clpfd10b
  "http://adventofcode.com/2016/day/4"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tcgen]
   [clojure.test.check.properties :as tcprop]

   [clojure.core.match :refer [match]]

   spyscope.core
   adventofcode1.spec-test-instrument-debug
   [clojure.core.logic.fd :as fd])
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some small helpers

(defn everyo [l f]
  (fresh [head tail]
    (conde
      [(== l ())]
      [(conso head tail l)
       (f head)
       (everyo tail f)])))


(defn fdsmallint
  [v]
  (fd/in v (fd/interval -100 100)))

(defn nonmembero
  [x l]
  (fresh [h t]
    (conde
      [(== l ())]
      [(conso h t l)
       (!= x h)
       (nonmembero x t)])))

(defn lengtho
  [l n]
  (fresh [h t m]
    (conde
      [(nilo l)(== n 0)]
      [(fd/> n 0)
       (conso h t l)
       (fd/+ m 1 n)
       (lengtho t m)])))

(defn sumo [l sum]
  (fresh [a d sum-of-remaining]
    (conde
      [(== l ()) (== sum 0)]
      [(conso a d l)
       (fd/+ a sum-of-remaining sum)
       (sumo d sum-of-remaining)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I am trying to make a version that can handle many shores
;;
;; I have places like 0,1,2...
;; I need to know I the man (or boat or elevetor) is there
;; If I am there, some restrictions might apply
;; If I am not there, some other restrictions might apply


(defn is_state
  [s]
  (fresh [man wolf goat cabbage]
    (== s [man wolf goat cabbage])
    (fd/in man wolf goat cabbage (fd/interval 0 1))))



(defn start_state
  [s]
  (sumo s 0))


(defn end_state
  [s]
  (sumo s 4))


(defn safe_goat
  [s]
  (fresh [man wolf goat cabbage]
    (== s [man wolf goat cabbage])
    (conde
      ;; different sides
      [(fd/!= wolf goat)]
      ;; same side as man
      [(== wolf man)(== goat man)])))

(defn safe_cabbage
  [s]
  (fresh [man wolf goat cabbage]
    (== s [man wolf goat cabbage])
    (conde
      ;; different sides
      [(fd/!= cabbage goat)]
      ;; same side as man
      [(== cabbage man)(== goat man)])))


;; toggle between 0 and 1
(defn change
  [x y]
  (fd/+ x y 1))



(defn move
  [s1, c, s2]
  (fresh [x y wolf goat cabbage]
    (== s1 [x wolf goat cabbage])
    (change x y)
    (conde
     [(== x wolf)
      (== c {:from x :who :wolf :to y})
      (== s2 [y y goat cabbage])]
     [(== x goat)
      (== c {:from x :who :goat :to y})
      (== s2 [y wolf y cabbage])]
     [(== x cabbage)
      (== c {:from x :who :cabbage :to y})
      (== s2 [y wolf goat y])]
     [(== c {:from x :who :nothing :to y})
      (== s2 [y wolf goat cabbage])])))

(defn solution
  [s1 path]
  (fresh [firstmove othermoves s2]
   (conde
    [(end_state s1)]
    [(conso firstmove othermoves path)
     (move s1 firstmove s2)
     (safe s2)
     (solution s2 othermoves)])))

(defn goal
  [n]
  (run* [path](lengtho path n) (fresh [s] (is_state s)(start_state s) (solution s path))))

(defn solution_unq
  [s1 path visited]
  (fresh [firstmove othermoves s2 visited2]
   (conde
    [(end_state s1)]
    [(conso firstmove othermoves path)
     (move s1 firstmove s2)
     (safe s2)
     (nonmembero s2 visited)
     (conso s2 visited visited2)
     (solution_unq s2 othermoves visited2)])))

(defn goal_unq
  [n]
  (run* [path](lengtho path n) (fresh [s] (is_state s)(start_state s) (solution_unq s path [s]))))

(defn main
  []
  (mw.utils/timed "foo" (goal_unq 7)))

(clojure.spec.test/instrument)

;; how to run all check functions for this namespace
;;
;; (-> (stest/enumerate-namespace 'adventofcode1.four2) stest/check)
;;
;; checked code of check, and it uses pmap already!

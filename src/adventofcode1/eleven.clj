(ns adventofcode1.eleven
  "http://adventofcode.com/2016/day/11"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tcgen]
   [clojure.test.check.properties :as tcprop]

   [clojure.core.match :refer [match]]

   mw.utils
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
  (fresh [e hg hm lg lm]
    (== s [e hg hm lg lm])
    ;; (fd/in e hg hm lg lm (fd/interval 1 4))
    (fd/in e hg hm lg lm (fd/domain 1 2 3 4))))



(defn start_state
  [s]
  ;; (sumo s 5)
  (== s [1 2 1 3 1]))



(defn end_state
  [s]
  (sumo s 20))

;; In other words, if a chip is ever left in the same area as another
;; RTG, and it's not connected to its own RTG, the chip will be
;; fried. Therefore, it is assumed that you will follow procedure and
;; keep chips connected to their corresponding RTG when they're in the
;; same room, and away from other RTGs otherwise.
(defn safe_h
  [s]
  (fresh [e hg hm lg lm]
    (== s [e hg hm lg lm])
    (conde
      ;; h in same room
      [(== hg hm)]
      ;; different room, but not same as other RTG
      [(!= hg hm)(!= hm lg)])))

(defn safe_l
  [s]
  (fresh [e hg hm lg lm]
    (== s [e hg hm lg lm])
    (conde
      ;; h in same room
      [(== lg lm)]
      ;; different room, but not same as other RTG
      [(!= lg lm)(!= lm hg)])))

;; lift can go up or down
(defn change
  [x y]
  (fd/in x y (fd/domain 1 2 3 4))
  (conde
   [(fd/+ x 1 y)]
   [(fd/- x 1 y)]))


;; Its capacity rating means it can carry at most yourself and two
;; RTGs or microchips in any combination. (They're rigged to some
;; heavy diagnostic equipment - the assembling machine will detach it
;; for you.) As a security measure, the elevator will only function if
;; it contains at least one RTG or microchip.
;;
;; i.e move 1 or two things
;; the possible combinations
;; hg
;; hm
;; lg
;; lm
;; hg hm
;; hg lg
;; hg lm
;; hm lg
;; hm lm
;; lg lm
(defn move
  [s1, c, s2]
  (fresh [x y hg hm lg lm]
    (== s1 [x hg hm lg lm])
    (change x y)
    (conde
     [(== x hg)
      (== c {:from x :who :hg :to y})
      (== s2 [y y hm lg lm])]
     [(== x hm)
      (== c {:from x :who :hm :to y})
      (== s2 [y hg y lg lm])]
     [(== x lg)
      (== c {:from x :who :lg :to y})
      (== s2 [y hg hm y lm])]
     [(== x lm)
      (== c {:from x :who :lm :to y})
      (== s2 [y hg hm lg y])]
     [(== x hg)(== x hm)
      (== c {:from x :who1 :hg :who2 :hm :to y})
      (== s2 [y y y lg lm])]
     [(== x hg)(== x lg)
      (== c {:from x :who1 :hg :who2 :lg :to y})
      (== s2 [y y hm y lm])]
     [(== x hg)(== x lm)
      (== c {:from x :who1 :hg :who2 :lm :to y})
      (== s2 [y y hm lg y])]
     [(== x hm)(== x lg)
      (== c {:from x :who1 :hm :who2 :lg :to y})
      (== s2 [y hg y y lm])]
     [(== x hm)(== x lm)
      (== c {:from x :who1 :hm :who2 :lm :to y})
      (== s2 [y hg y lg y])]
     [(== x lg)(== x lm)
      (== c {:from x :who1 :lg :who2 :lm :to y})
      (== s2 [y hg hm y y])])
    ;; why do I need this?
    ;; if I do not have that, the elevator will go to the 8th floor or more :-)
    ;; {:from 5, :who :hm, :to 6}
    ;; is it that change is called too early?
    (is_state s2)))





(defn solution
  [s1 path visited]
  (fresh [firstmove othermoves s2 visited2]
    (conde
     ;; debug [(end_state s1)]
     ;; [(== 1 1)]
     [(end_state s1)]
     [(conso firstmove othermoves path)
      (move s1 firstmove s2)
      (safe_h s2)
      (safe_l s2)
      (nonmembero s2 visited)
      (conso s2 visited visited2)
      (solution s2 othermoves visited2)])))

(defn goal
  [n]
  (run 1 [path](lengtho path n) (fresh [s] (is_state s)(start_state s) (solution s path [s]))))

;; (count (last (run 1 [path](lengtho path 11) (fresh [s] (is_state s)(start_state s) (solution s path [s])))))
;; => 11
;; even if I place goal 9 below, where is the path extended?
(defn main
  []
  (mw.utils/timed "foo" (goal 11)))

(clojure.spec.test/instrument)

;; how to run all check functions for this namespace
;;
;; (-> (stest/enumerate-namespace 'adventofcode1.four2) stest/check)
;;
;; checked code of check, and it uses pmap already!

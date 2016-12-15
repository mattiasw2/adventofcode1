(ns adventofcode1.clpfd10
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


;; different libraries for clpfd and lp
;; https://github.com/clojure/core.logic
;;
;; https://spin.atomicobject.com/2015/12/14/logic-programming-clojure-finite-domain-constraints/
;; https://github.com/clojure/core.logic/wiki
;;
;; jacop:
;; https://github.com/aengelberg/clocop
;;
;; choco: http://www.choco-solver.org/
;;

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (all
        (if-not (zero? hint)
          (== (first vars) hint)
          succeed)
        (init (next vars) (next hints))))
    succeed))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar)
        rows (->> vars (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 9 3)
                   y (range 0 9 3)]
               (get-square rows x y))]
    (run 1 [q]
      (== q vars)
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
      (init vars hints)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct sqs))))

(def hints
  [2 0 7 0 1 0 5 0 8
   0 0 0 6 7 8 0 0 0
   8 0 0 0 0 0 0 0 6
   0 7 0 9 0 6 0 5 0
   4 9 0 0 0 0 0 1 3
   0 3 0 4 0 1 0 2 0
   5 0 0 0 0 0 0 0 1
   0 0 0 2 9 4 0 0 0
   3 0 6 0 8 0 4 0 9])

;; (sudokufd hints)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn everyo [l f]
  (fresh [head tail]
    (conde
      [(== l ())]
      [(conso head tail l)
       (f head)
       (everyo tail f)])))

(defn reverso [l r]
  (conde
    [(== l ()) (== r ())]
    [(fresh [la ld ldr]
      (conso la ld l)
      (appendo ldr (list la) r)
      (reverso ld ldr))]))

(defn palindromo [v]
  (fresh [r]
    (== v r)
    (reverso v r)))

;; and now some constraints

(defn sumo [l sum]
  (fresh [a d sum-of-remaining]
    (conde
      [(== l ()) (== sum 0)]
      [(conso a d l)
       (fd/+ a sum-of-remaining sum)
       (sumo d sum-of-remaining)])))

(defn find-palindromes-totalling [sum results]
  (let [domain (fd/interval 1 1000)]
    (run results [q]
      (palindromo q)
      (everyo q #(fd/in % domain))
      (sumo q sum))))

;; (find-palindromes-totalling 20 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adventofcode1.clpfd10>  (run* [q])
;;    (== q 1)
;;    (== q 2)
;; ()
;; adventofcode1.clpfd10>  (run* [q](membero q [1 2 3]))
;; (1 2 3)
;; adventofcode1.clpfd10>  (run* [q](fd/in q (fd/interval 0 100))(membero q [1 2 3]))
;; (1 2 3)
;; adventofcode1.clpfd10>  (run* [q](fd/in q (fd/interval 0 100))(membero q [1 2 3])(fd/+ q 1 3))
;; (2)
;; adventofcode1.clpfd10>

(defn fdint
  [v]
  (fd/in v (fd/interval -100 100)))

(defn plus1is3
  [x]
  (fd/+ x 1 3))

(run* [q](fdint q)(plus1is3 q))

;; not working as I expect
;; (run* [q](== q 1)(nonmembero q [1 2 3]))  => (1)
;; (defne nonmembero
;;   "A relation where l is a collection, such that l does not contain x"
;;   [x l]
;;   ([_ []])
;;   ([_ [head . tail]
;;      (!= x head)
;;      (nonmembero x tail)]))

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


;;; trying to model wolf goat cabbage
;; https://ucsd-progsys.github.io/cse130/static/goat_etc.html
;; change(e,w).
;; change(w,e).

(defn change
  [x y]
  (conde
    [(== x 0)(== y 1)]
    [(== x 1)(== y 0)]))

;; move([X,X,Goat,Cabbage],wolf,[Y,Y,Goat,Cabbage]) :- change(X,Y).
;; move([X,Wolf,X,Cabbage],goat,[Y,Wolf,Y,Cabbage]) :- change(X,Y).
;; move([X,Wolf,Goat,X],cabbage,[Y,Wolf,Goat,Y]) :- change(X,Y).
;; move([X,Wolf,Goat,Cabbage],nothing,[Y,Wolf,Goat,Cabbage]) :- change(X,Y).

(defn move
  [s1, c, s2]
  (fresh [x y wolf goat cabbage]
    (conde
     [(== s1 [x x goat cabbage])
      (== c :wolf)
      (== s2 [y y goat cabbage])
      (change x y)]
     [(== s1 [x wolf x cabbage])
      (== c :goat)
      (== s2 [y wolf y cabbage])
      (change x y)]
     [(== s1 [x wolf goat x])
      (== c :cabbage)
      (== s2 [y wolf goat y])
      (change x y)]
     [(== s1 [x wolf goat cabbage])
      (== c :nothing)
      (== s2 [y wolf goat cabbage])
      (change x y)])))

;; oneEq(X,X,_).
;; oneEq(X,_,X).

(defn oneEq
  [x y z]
  (conde
   [(== x y)]
   [(== x z)]))

;; safe([Man,Wolf,Goat,Cabbage]) :-
;;    oneEq(Man,Goat,Wolf),
;;    oneEq(Man,Goat,Cabbage).

(defn safe
  [s1]
  (fresh [man wolf goat cabbage]
    (conde
     [(== s1 [man wolf goat cabbage])
      (oneEq man goat wolf)
      (oneEq man goat cabbage)])))


;; solution([e,e,e,e],[]).
;; solution(Config,[FirstMove|OtherMoves]) :-
;;    move(Config,Move,NextConfig),
;;    safe(NextConfig),
;;    solution(NextConfig,OtherMoves).

(defn solution
  [s1 path]
  (fresh [firstmove othermoves s2]
   (conde
    [(== s1 [1 1 1 1])]
    [(conso firstmove othermoves path)
     (move s1 firstmove s2)
     (safe s2)
     (solution s2 othermoves)])))

(defn goal
  [n]
  (run* [path](lengtho path n) (solution [0 0 0 0] path)))

(defn solution_unq
  [s1 path visited]
  (fresh [firstmove othermoves s2 visited2]
   (conde
    [(== s1 [1 1 1 1])]
    [(conso firstmove othermoves path)
     (move s1 firstmove s2)
     (safe s2)
     (nonmembero s2 visited)
     (conso s2 visited visited2)
     (solution_unq s2 othermoves visited2)])))

(defn goal_unq
  [n]
  (run* [path](lengtho path n) (solution_unq [0 0 0 0] path [[0 0 0 0]])))

;; very similar performance
;;
;; (mw.utils/timed "foo" (goal_unq 7))
;; "Timed: foo goal_unq: 0.180543 msecs"

;;  (mw.utils/timed "foo" (goal 7))
;; "Timed: foo goal: 0.179358 msecs"

;; ?- length(X,7), solution([w,w,w,w],X).

;; X = [goat, nothing, wolf, goat, cabbage, nothing, goat]


(clojure.spec.test/instrument)

;; how to run all check functions for this namespace
;;
;; (-> (stest/enumerate-namespace 'adventofcode1.four2) stest/check)
;;
;; checked code of check, and it uses pmap already!

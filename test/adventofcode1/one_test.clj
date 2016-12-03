(ns adventofcode1.one-test
  (:require
   ;; [schema.core :as s]
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.one :refer :all]
   )
  )


;; Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
;; R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
;; R5, L5, R5, R3 leaves you 12 blocks away.
(deftest total-distance-test
  (is (= 5 (total-distance [{:r 2}{:l 3}])))
  (is (= 2 (total-distance [{:r 2}{:r 2}{:r 2}])))
  (is (= 0 (total-distance [{:r 2}{:r 0}{:r 2}])))
  (is (= 2 (total-distance [{:r 2}{:r 0}{:r 2}{:r 2}])))
  (is (= 12 (total-distance [{:r 5}{:l 5}{:r 5}{:r 3}])))
  )


;; R2, L3, R2, R4, L2, L1, R2, R4, R1, L4, L5, R5, R5, R2, R2, R1, L2, L3, L2, L1, R3, L5, R187, R1, R4, L1, R5, L3, L4, R50, L4, R2, R70, L3, L2, R4, R3, R194, L3, L4, L4, L3, L4, R4, R5, L1, L5, L4, R1, L2, R4, L5, L3, R4, L5, L5, R5, R3, R5, L2, L4, R4, L1, R3, R1, L1, L2, R2, R2, L3, R3, R2, R5, R2, R5, L3, R2, L5, R1, R2, R2, L4, L5, L1, L4, R4, R3, R1, R2, L1, L2, R4, R5, L2, R3, L4, L5, L5, L4, R4, L2, R1, R1, L2, L3, L2, R2, L4, R3, R2, L1, L3, L2, L4, L4, R2, L3, L3, R2, L4, L3, R4, R3, L2, L1, L4, R4, R2, L4, L4, L5, L1, R2, L5, L2, L3, R2, L2

(def puzzle-1
  [{:r 2}, {:l 3}, {:r 2}, {:r 4}, {:l 2}, {:l 1}, {:r 2}, {:r 4}, {:r 1}, {:l 4}, {:l 5}, {:r 5}, {:r 5}, {:r 2}, {:r 2}, {:r 1}, {:l 2}, {:l 3}, {:l 2}, {:l 1}, {:r 3}, {:l 5}, {:r 187}, {:r 1}, {:r 4}, {:l 1}, {:r 5}, {:l 3}, {:l 4}, {:r 50}, {:l 4}, {:r 2}, {:r 70}, {:l 3}, {:l 2}, {:r 4}, {:r 3}, {:r 194}, {:l 3}, {:l 4}, {:l 4}, {:l 3}, {:l 4}, {:r 4}, {:r 5}, {:l 1}, {:l 5}, {:l 4}, {:r 1}, {:l 2}, {:r 4}, {:l 5}, {:l 3}, {:r 4}, {:l 5}, {:l 5}, {:r 5}, {:r 3}, {:r 5}, {:l 2}, {:l 4}, {:r 4}, {:l 1}, {:r 3}, {:r 1}, {:l 1}, {:l 2}, {:r 2}, {:r 2}, {:l 3}, {:r 3}, {:r 2}, {:r 5}, {:r 2}, {:r 5}, {:l 3}, {:r 2}, {:l 5}, {:r 1}, {:r 2}, {:r 2}, {:l 4}, {:l 5}, {:l 1}, {:l 4}, {:r 4}, {:r 3}, {:r 1}, {:r 2}, {:l 1}, {:l 2}, {:r 4}, {:r 5}, {:l 2}, {:r 3}, {:l 4}, {:l 5}, {:l 5}, {:l 4}, {:r 4}, {:l 2}, {:r 1}, {:r 1}, {:l 2}, {:l 3}, {:l 2}, {:r 2}, {:l 4}, {:r 3}, {:r 2}, {:l 1}, {:l 3}, {:l 2}, {:l 4}, {:l 4}, {:r 2}, {:l 3}, {:l 3}, {:r 2}, {:l 4}, {:l 3}, {:r 4}, {:r 3}, {:l 2}, {:l 1}, {:l 4}, {:r 4}, {:r 2}, {:l 4}, {:l 4}, {:l 5}, {:l 1}, {:r 2}, {:l 5}, {:l 2}, {:l 3}, {:r 2}, {:l 2}])

;; 246 was the correct answer
(deftest puzzle-1-test
  (is (= 246 (total-distance adventofcode1.one-test/puzzle-1))))


(deftest steps-same-position-test
  (is (= "No easter bunny found" (steps-same-position [{:r 2}{:l 3}])))
  (is (= "No easter bunny found" (steps-same-position [{:r 2}{:r 2}{:r 2}])))
  (is (= "No easter bunny found" (steps-same-position [{:r 5}{:l 5}{:r 5}{:r 3}])))
  ;; bad tests, since zero steps means same position, so abort
  (is (= 2 (steps-same-position [{:r 2}{:r 0}{:r 2}])))
  (is (= 2 (steps-same-position [{:r 2}{:r 0}{:r 2}{:r 2}])))
  ;; more realistic sample
  (is (= 0 (steps-same-position [{:r 2}{:r 2}{:r 2}{:r 2}])))
  (is (= 0 (steps-same-position [{:r 2}{:r 2}{:r 2}{:r 2}{:r 5}])))
  ;; and their sample
  (is (= 4 (steps-same-position [{:r 8}{:r 4}{:r 4}{:r 8}])))
  )

(deftest puzzle-1b-test
  (is (= 251 (steps-same-position adventofcode1.one-test/puzzle-1))))



;; adventofcode1.one> (turn-left north)
;; {:x -1, :y 0}
;; adventofcode1.one> (straight {:x 0 :y 1} {:x 1 :y 1} 10)
;; {:x 10, :y 11}
;; adventofcode1.one> (straight {:x 0 :y 1} {:x 1 :y 1} 0)
;; {:x 0, :y 1}
;; adventofcode1.one> (s/conform (s/int-in -10 10) -10)
;; -10
;; adventofcode1.one> (s/conform (s/int-in -10 10) 10)
;; :clojure.spec/invalid
;; adventofcode1.one> (s/conform (s/int-in -10 10) 9)
;; 9
;; adventofcode1.one> (s/conform ::xy {:x -100 :y 100})
;; :clojure.spec/invalid
;; adventofcode1.one> (s/conform ::xy {:x -100 :y 99})
;; {:x -100, :y 99}
;; adventofcode1.one> (gen/generate (s/gen ::path-un))
;; ({:r 66} {:l 94} {:l 74} {:l 88} {:r 99} {:l 4} {:r 55} {:l 75} {:r 56} {:r 57} {:r 87} {:l 70} {:r 56} {:r 50} {:l 69} {:r 47} {:r 55} {:l 1} {:r 61} {:l 5} {:l 77} {:r 59} {:r 6})
;; adventofcode1.one> (s/conform ::path-un '({:r 66} {:l 94} {:l 74} {:l 88} {:r 99} {:l 4} {:r 55} {:l 75} {:r 56} {:r 57} {:r 87} {:l 70} {:r 56} {:r 50} {:l 69} {:r 47} {:r 55} {:l 1} {:r 61} {:l 5} {:l 77} {:r 59} {:r 6}))
;; [[:turn-right {:r 66}] [:turn-left {:l 94}] [:turn-left {:l 74}] [:turn-left {:l 88}] [:turn-right {:r 99}] [:turn-left {:l 4}] [:turn-right {:r 55}] [:turn-left {:l 75}] [:turn-right {:r 56}] [:turn-right {:r 57}] [:turn-right {:r 87}] [:turn-left {:l 70}] [:turn-right {:r 56}] [:turn-right {:r 50}] [:turn-left {:l 69}] [:turn-right {:r 47}] [:turn-right {:r 55}] [:turn-left {:l 1}] [:turn-right {:r 61}] [:turn-left {:l 5}] [:turn-left {:l 77}] [:turn-right {:r 59}] [:turn-right {:r 6}]]
;; adventofcode1.one> (s/conform ::path {::r 10})
;; :clojure.spec/invalid
;; adventofcode1.one> (s/conform ::path '({::r 10}))
;; [[:turn-right #:adventofcode1.one{:r 10}]]
;; adventofcode1.one> (gen/generate (spec/gen ::path))
;; CompilerException java.lang.RuntimeException: No such namespace: spec, compiling:(*cider-repl adventofcode1*<2>:655:34)
;; adventofcode1.one> (gen/generate (s/gen ::path))
;; (#:adventofcode1.one{:l 99} #:adventofcode1.one{:r 83} #:adventofcode1.one{:r 82} #:adventofcode1.one{:l 89} #:adventofcode1.one{:r 96} #:adventofcode1.one{:r 5} #:adventofcode1.one{:r 18} #:adventofcode1.one{:r 96} #:adventofcode1.one{:r 91} #:adventofcode1.one{:l 57} #:adventofcode1.one{:l 69} #:adventofcode1.one{:l 92} #:adventofcode1.one{:r 66} #:adventofcode1.one{:r 58} #:adventofcode1.one{:l 73} #:adventofcode1.one{:l 54} #:adventofcode1.one{:r 3} #:adventofcode1.one{:r 73} #:adventofcode1.one{:l 55} #:adventofcode1.one{:l 70} #:adventofcode1.one{:r 93} #:adventofcode1.one{:r 51} #:adventofcode1.one{:r 79} #:adventofcode1.one{:l 88} #:adventofcode1.one{:l 58})
;; adventofcode1.one> (gen/generate (s/gen ::path-un))
;; ({:r 61} {:r 51} {:l 50} {:r 62} {:l 74} {:l 93})
;; adventofcode1.one> (straight {:x 0 :y 1} north {:l 10})
;; ClassCastException clojure.lang.PersistentArrayMap cannot be cast to java.lang.Number  clojure.lang.Numbers.multiply (Numbers.java:148)
;; adventofcode1.one> (straight {:x 0 :y 1} north 10)
;; {:x 0, :y 11}
;; adventofcode1.one> (straight {:x 0 :y 1} north "d")

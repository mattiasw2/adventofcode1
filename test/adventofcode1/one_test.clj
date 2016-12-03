(ns adventofcode1.one-test
  (:require
   ;; [schema.core :as s]
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   ;; You can also call (timbre/refer-timbre) to configure Clj ns referrals automatically
   [taoensso.timbre :as timbre
    :refer (log  trace  debug  info  warn  error  fatal  report
                 logf tracef debugf infof warnf errorf fatalf reportf
                 spy get-env log-env)]
   spyscope.core
   [adventofcode1.one :refer :all]
   )
  )

;; (do (set! *warn-on-reflection* true)
;;     (set! *unchecked-math* :warn-on-boxed)
;;     (schema.core/set-fn-validation! true))



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

(ns adventofcode1.three
  "http://adventofcode.com/2016/day/3"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]

   [clojure.core.match :refer [match]]

   spyscope.core
   adventofcode1.spec-test-instrument-debug
   )
  )


(defn valid-triangle?
  "Return true if these three side length is a triangle"
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))


(defn count-valid-triangles
  [triangles]
  (reduce + (map #(if (valid-triangle? %) 1 0) triangles)))

(clojure.spec.test/instrument)

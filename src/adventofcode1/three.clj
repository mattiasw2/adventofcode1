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

;; Prevent issues like
;; :smallest [([[-6856367198276841078 -2367004838577934731 0]])]}}, :sym adventofcode1.three/count-valid-triangles-horizontally, :failure #error {
;; :cause "integer overflow"
;; (s/def ::my-int (s/and integer? #(< (Math/abs %) 4000000000)))
(s/def ::my-int (s/int-in -4000000000 4000000000))
;; (s/def ::my-int integer?)

(s/fdef valid-triangle?
        ;; do not add ":kind vector?" since extremely slow then
        :args (s/cat :abc (s/coll-of ::my-int :count 3))
        :ret  boolean?)

(defn valid-triangle?
  "Return true if these three side length is a triangle"
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))


(s/fdef count-valid-triangles-horizontally
        :args (s/cat :abc-list (s/coll-of (s/coll-of ::my-int :count 3)))
        :ret  integer?)


(defn count-valid-triangles-horizontally
  [triangles]
  (reduce + (map #(if (valid-triangle? %) 1 0) triangles)))

(clojure.spec.test/instrument)

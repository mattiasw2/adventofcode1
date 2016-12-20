(ns adventofcode1.seventeen-2015
  "http://adventofcode.com/2015/day/17"
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
   adventofcode1.spec-test-instrument-debug))


(defn count-solutions
  [left buckets]
  (if (zero? left) 1
      (if (empty? buckets) 0
          (let [use-me (first buckets)
                left2  (- left use-me)]
            (if (>= left2 0)
              (+ (count-solutions left2 (rest buckets))
                 (count-solutions left  (rest buckets)))
              0)))))


(defn count-shortest-solutions-2
  [g depth left buckets]
  (if (zero? left)
    (do (if (< depth (:size @g))
          (swap! g (fn [x]{:size depth :count 0})))
        (if (= depth (:size @g))
          (swap! g (fn [x]{:size (:size x) :count (inc (:count x))})))
        0)
    (if (empty? buckets) 0
          (let [use-me (first buckets)
                left2  (- left use-me)]
            (if (>= left2 0)
              (+ (count-shortest-solutions-2 g (inc depth) left2 (rest buckets))
                 (count-shortest-solutions-2 g depth left  (rest buckets)))
              0)))))

(defn count-shortest-solutions
  [left buckets]
  (let [res (atom {:size 100000 :count 0})]
    (count-shortest-solutions-2 res 0 left buckets)
    @res))

(def start-state (sort [50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40]))

(def demo-state (sort [20, 15, 10, 5, 5]))

(defn puzzle-demo-first
  []
  (count-solutions 25 demo-state))

(defn puzzle-demo-second
  []
  (count-shortest-solutions 25 demo-state))

(defn puzzle-first
  []
  (count-solutions 150 start-state))

(defn puzzle-second
  []
  (count-shortest-solutions 150 start-state))

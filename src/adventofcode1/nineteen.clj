(ns adventofcode1.nineteen
  "http://adventofcode.com/2016/day/19"
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
   adventofcode1.spec-test-instrument-debug))

(defn init-arr
  [arr size v]
  (doseq [ii (range 0 size)]
    (aset arr ii v)))

;; (ex-info "The ice cream has melted!"
;;        {:causes             #{:fridge-door-open :dangerously-high-temperature}
;;         :current-temperature {:value 25 :unit :celsius}})

(defn find-next-elf
  ""
  [arr size taker giver]
  (if (>= giver size) (recur arr size taker 0)
   (if (= taker giver)(throw (Exception. (str "taker #" (inc taker) " has " (aget arr taker) " presents.")))
      (if (pos? (aget arr giver))
        (do
          (aset arr taker (+ (aget arr taker)(aget arr giver)))
          (aset arr giver 0)
          ;; return the elf that might have any presents
          (inc giver))
        (recur arr size taker (inc giver))))))


(defn move
  "Starting at idx, find a non-0 elf, and make it take a present from the left.
   Remember not too look to far, i.e. max to next idx."
  [arr size idx]
  (if (>= idx size) (recur arr size 0)
      (if (zero? (aget arr idx)) (recur arr size (inc idx))
        ;; now we find elf that can take something
        (let [next-elf (find-next-elf arr size idx (inc idx))]
          (recur arr size next-elf)))))

(defn main
  [size]
  (let [arr (make-array Integer/TYPE size)]
    (init-arr arr size 1)
    ;; need to add 1 to result, since elves are 1-based
    (inc (move arr size 0))))

(defn puzzle-a
  []
  (main 3014603))

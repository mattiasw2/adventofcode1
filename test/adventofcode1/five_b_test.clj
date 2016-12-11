(ns adventofcode1.five-b-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.five-b :refer :all]))


;; (deftest next-chars-tmp-test
;;   (is (= {:pos 1 :chr 1}
;;          (take 1 (next-chars-temp "abc" 3231900)))))

;; (deftest next-chars-tmp-test
;;   (is (= '(3231929 5017308 5278568)
;;          (take 3 (adventofcode1.five/next-chars-temp "abc" 3231900)))))

;; (deftest next-chars-1-test
;;   (is (= ["1"]
;;          (take 1 (adventofcode1.five/next-chars "abc" 3231900)))))

;; (deftest next-chars-2-test
;;   (is (= ["1" "8" "f"]
;;          (take 3 (adventofcode1.five/next-chars "abc" 3231900)))))

(deftest puzzle-5-b-test
   (is (= "999828ec"
          (puzzle-five-b 40 "cxdnnyjw"))))


;; Error in puzzle-5-b-test
;; expected: (= "f77a0e6e" (puzzle-five-b 30 "cxdnnyjw"))
;;    error: java.lang.AssertionError: Assert failed: not enough values found: clojure.core$_PLUS_@5fd7db92("9" nil "9" "8" "2" "8" "e" "c")
;; (every? (fn* [p1__26941#] (not (nil? p1__26941#))) (seq res))

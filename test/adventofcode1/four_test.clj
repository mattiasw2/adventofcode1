(ns adventofcode1.four-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]

   spyscope.core
   [adventofcode1.four :refer :all]
   )
  )

;; aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
;; a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
;; not-a-real-room-404[oarel] is a real room.
;; totally-real-room-200[decoy] is not.

(def test-samples
  [
   "aaaaa-bbb-z-y-x-123[abxyz]"
   "a-b-c-d-e-f-g-h-987[abcde]"
   "not-a-real-room-404[oarel]"
   "totally-real-room-200[decoy]"
   ])

(deftest split-room-name-test
  (is (= '("aaaaa-bbb-z-y-x-" "123" "abxyz") (split-room-name "aaaaa-bbb-z-y-x-123[abxyz]")))
  )

(deftest checksum-test
  (is (= "abzyx" (checksum "aaaaa-bbb-z-y-x-")))
  )

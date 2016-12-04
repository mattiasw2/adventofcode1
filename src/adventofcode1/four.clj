(ns adventofcode1.four
  "http://adventofcode.com/2016/day/4"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]

   [clojure.core.match :refer [match]]

   spyscope.core
   adventofcode1.spec-test-instrument-debug
   )
  )

(s/fdef split-room-name
        :args (s/cat :name string?)
        ;; tuple with [name, sectorid, checksum]
        :ret  (s/coll-of string? :count 3))

(defn split-room-name
  "Split room name and return tuple with [name, sectorid, checksum] as strings."
  [name]
  (rest (re-find (re-matcher #"([-a-z]+)(\d+)\[(.+)\]" name))))

(s/def ::name string?)
(s/def ::checksum string?)
(s/def ::sectorid integer?)
(s/def ::parsed-room-name (s/keys :req-un [::name ::checksum ::sectorid]))

(s/fdef parse-room-name
        :args ::name
        :ret  ::parsed-room-name)

(defn parse-room-name
  "Split and parse room name and return map with :name :sectorid :checksum"
  [name]
  (let [split (split-room-name name)
        _ (assert (= 3 (count split)) "Not a valid room name")
        [name sectorid checksum] split]
    {:name name
     ;; parse number using bigint, could have used read-string instead, but then I need to check that result is a number
     :sectorid (bigint sectorid)
     :checksum checksum}))

(s/fdef checksum
        :args (s/cat :name ::name)
        :ret  ::checksum)

(defn checksum
  [name]
  (let [freq (dissoc (frequencies (seq name)) \-)
        checksum (take 5 (reverse (sort (map (fn [[char cnt]] [cnt char]) freq))))]
    (clojure.string/join (map second checksum))))


(clojure.spec.test/instrument)

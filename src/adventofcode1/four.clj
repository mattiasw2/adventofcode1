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

;; Improve: using ::name both for "aaaaa-bbb-z-y-x-123[abxyz]" and "aaaaa-bbb-z-y-x-"
(s/def ::name string?)
(s/def ::checksum string?)
(s/def ::sectorid integer?)
(s/def ::parsed-room-name (s/keys :req-un [::name ::checksum ::sectorid]))

(s/fdef parse-room-name
        :args (s/cat :name ::name)
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

(defn build-result
  "Checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization."
  [acc distinct-keys checksum]
  (if (empty? distinct-keys)
    ;; we cannot guarantee there will not be more than 5 chars, just keep the first ones
    (take 5 acc)
    (let [res (filter (fn [[cnt char]] (= cnt (first distinct-keys))) checksum)]
      (recur (concat acc res) (rest distinct-keys) checksum))))

(s/fdef checksum
        :args (s/cat :name ::name)
        :ret  ::checksum)

(defn checksum
  [name]
  (let [freq (dissoc (frequencies (seq name)) \-)
        checksum (sort (map (fn [[char cnt]] [cnt char]) freq))
        ;; [] is just an accumulator to make function tail-recursive
        ;; (distinct (map first checksum)) is the letters in occurrency order
        ;; (reverse checksum) is the order we are going to show them if tie
        res (build-result [] (reverse (distinct (map first checksum))) checksum)]
    (clojure.string/join (map second res))))


(defn valid-room-name
  "Check if room name is valid.
   If valid, return sectorid, else 0"
  [name]
  (let [room (parse-room-name name)
        checksum (checksum (:name room))]
    (if (= checksum (:checksum room))
      (:sectorid room)
      0)))

(defn sum-sectorid-of-valid-room-names
  [names]
  (reduce + (map valid-room-name names)))

(defn valid-room-name?
  "Return true if room is valid."
  [name]
  (> (valid-room-name name) 0))


(clojure.spec.test/instrument)

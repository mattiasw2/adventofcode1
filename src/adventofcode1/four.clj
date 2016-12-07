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
  [acc distinct-cnts-desc checksum]
  (if (empty? distinct-cnts-desc)
    ;; we cannot guarantee there will not be more than 5 chars, just keep the first ones
    (take 5 acc)
    (let [res (filter (fn [[cnt char]] (= cnt (first distinct-cnts-desc))) checksum)]
      (recur (concat acc res) (rest distinct-cnts-desc) checksum))))

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

(defn valid-room-name?
  "Return true if room is valid."
  [name]
  (try
    (> (valid-room-name name) 0)
    (catch Throwable _ false)))


(defn sum-sectorid-of-valid-room-names
  [names]
  (reduce + (map valid-room-name names)))

(s/fdef rotate-char
        ;; we need to require a pos? or zero? here since negative values doesn't work
        :args (s/cat :text char? :n (s/and integer? (s/or :zero zero? :pos pos?)))
        :ret  char?)

(defn rotate-char
  "Rotate the lower-case char n positions,"
  [ch n]
  (let [res (+ (int ch) n)]
    (char (if (> res 122) (+ 97 (mod (- res 97) 26)) res))))

(s/fdef rotate-string
        :args (s/cat :text string? :n (s/and integer? (s/or :zero zero? :pos pos?)))
        :ret  string?)

(defn rotate-string
  "Rotate each char in lower-case string n positions."
  [text n]
  (clojure.string/join (map #(rotate-char % n) (or (seq text) []))))

(s/fdef find-northpole
        ;; need a better way to generate valid room names
        ;; ExceptionInfo Couldn't satisfy such-that predicate after 100 tries.  clojure.core/ex-info (core.clj:4725)
        :args (s/cat :names (s/coll-of (s/and string? valid-room-name?)))
        :ret  (s/or :not-found nil :found (s/tuple integer? string?)))


(defn find-northpole
  [names]
  (if (empty? names) nil
    (let [room (parse-room-name (first names))
          rotated (rotate-string (:name room) (:sectorid room))]
      (if (clojure.string/starts-with? rotated "north")
        [(:sectorid room) rotated]
        (recur (rest names))))))




(clojure.spec.test/instrument)

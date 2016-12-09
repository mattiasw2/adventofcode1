(ns adventofcode1.four
  "http://adventofcode.com/2016/day/4"
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
        ;; :args (s/cat :names (s/coll-of (s/and string? valid-room-name?)))
        ;; maybe I should look at https://github.com/gfredericks/test.chuck
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

#_(stest/check `find-northpole)

;;;;;;;;;;;;;;;;;;;;;;;;;


;;; using test.check's gen
#_(def sort-idempotent-prop-tc
  (prop/for-all [v (gen/vector tcgen/int)]
                (= (sort v) (sort (sort v)))))

;; (tc/quick-check 100 sort-idempotent-prop-tc)

;; using specs gen
#_(def sort-idempotent-prop
  (prop/for-all [v (gen/vector (gen/int))]
                (= (sort v) (sort (sort v)))))

;; (tc/quick-check 100 sort-idempotent-prop)

;; using s/gen
#_(def sort-idempotent-prop-spec
  (prop/for-all [v (s/gen (s/coll-of int?))]
                (= (sort v) (sort (sort v)))))


;; (tc/quick-check 100 sort-idempotent-prop-spec)

;; instead of using s/coll-of, you can use s/spec
;; (gen/sample (s/gen (s/cat :foo int? :cols (s/spec (s/+ string?)))))

;; how to get generate :args from a fdef spec
;; the trick is `split-room-name or #', refering to the ::split-room-name will not work

;; (gen/sample (s/gen (:args (s/get-spec `split-room-name))))
;; (("") ("") ("dG") ("PF") ("NhM") ("B7H") ("8i") ("") ("w") ("dbFbM"))
;; adventofcode1.four> (gen/sample (s/gen (:args (s/get-spec #'split-room-name))))
;; (("") ("") ("Ib") ("4") ("ub") ("g4a") ("2R2i0") ("ZM") ("lCg6Xzm3") ("w"))
;; adventofcode1.four> (gen/sample (s/gen (:args (s/get-spec #'split-room-name))))
;; (("") ("") ("2") ("8") ("p") ("xTZ") ("aG7g07") ("QAIw10o") ("f") (""))
;; adventofcode1.four> (gen/sample (s/gen (:ret (s/get-spec #'split-room-name))))
;; (["" "" ""] ["G" "1" "0"] ["" "M" "95"] ["mW" "M7T" "3"] ["p60H" "U" ""] ["2" "d1Mk7" "yG"] ["X58p" "N24llA" "W"] ["rS5c8Z" "Ze" "7z43LCy"] ["AFey9sl" "w827" ""] ["IW08KGg" "j" "ic"])
;; adventofcode1.four> (gen/sample (s/gen (:ret (s/get-spec #'split-room-name))))
;; (["" "" ""] ["Z" "k" ""] ["" "5O" "45"] ["b4k" "4" "ZL"] ["" "V" ""] ["il" "1M0b" "O074"] ["E" "SzL7" "C321"] ["R5" "" "0928F"] ["f1f4" "36DmP2P" "oh"] ["W6c" "l3GjgsHt" "3P"])
;; adventofcode1.four> (gen/sample (s/gen (:ret (s/get-spec #'split-room-name))))
;; (["" "" ""] ["" "" ""] ["1" "" "9H"] ["1" "Gu2" "32"] ["5RO" "a" "p"] ["6lxy" "Z7E" "SO3Mv"] ["42Q" "7CEA0" "i8"] ["YnT6HS" "" ""] ["NfEYQW" "i0hGId" "r8oj"] ["ZHw" "HU0D40An5" "34eLgU"])
;; adventofcode1.four> (gen/sample (s/gen (s/get-spec ::name)))

;; ("" "" "9Y" "a3a" "j" "H923x" "E" "yxd" "7172c" "lFYt")
;; adventofcode1.four>


;; yes map + apply of gen/sample works fine
;;
;; (map #(do (println %)(apply rotate-char %)) (gen/sample (s/gen (:args (s/get-spec #'rotate-char))) 3))
;; (> 0)
;; (a 0)
;; (a 0)
;; (\> \g \a)

#_(clojure.spec.test/instrument)

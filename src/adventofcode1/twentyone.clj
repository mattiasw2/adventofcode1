(ns adventofcode1.twentyone
  "http://adventofcode.com/2016/day/21"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tcgen]
   [clojure.test.check.properties :as tcprop]

   [clojure.core.match :refer [match]]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]

   spyscope.core
   mw.utils
   adventofcode1.spec-test-instrument-debug))

(defn parse-int
  [s]
  (int (bigint s)))

(defn catvec
  [a b]
  (vec (concat a b)))

(defn remove-at
  "Remove cell at idx in arr."
  [arr idx]
  (catvec (subvec arr 0 idx) (subvec arr (inc idx))))

(defn insert-at
  "Insert cell at idx in arr."
  [arr idx v]
  (catvec (subvec arr 0 idx) (catvec [v] (subvec arr idx))))


;;; for 2nd puzzle
;;; reversable operations
;;; swap-position
;;; reverse-positions
;;; rotate-right
;;; rotate-left
;;; swap-letter
;;;
;;; non-reversible operations
;;; move-position
;;; rotate-based-on




;; move position X to position Y means that the letter which is at
;; index X should be removed from the string, then inserted such that
;; it ends up at index Y.

;; move position 1 to position 4 removes the letter at position 1 (c), then inserts it at position 4 (the end of the string): bdeac.
;; move position 3 to position 0 removes the letter at position 3 (a),
;; then inserts it at position 0 (the front of the string): abdec.
;; (move-position "bcdea" 1 4) => "bdeac"
;; (move-position "bdeac" 3 0) => "abdec"
(defn move-position
  [text fr to]
  (let [arr (vec (seq text))]
    (str/join (insert-at (remove-at arr fr) to (nth arr fr)))))


;; move position 2 to position 1
(defn parse-move-position
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr to] (re-find (re-matcher #"^move position (\d+) to position (\d+)$" cmd))]
    (list move-position text (parse-int fr) (parse-int to))))

;; swap position X with position Y means that the letters at indexes X
;; and Y (counting from 0) should be swapped.
(defn swap-position
  [text fr to]
  (let [arr (vec (seq text))
        cur (nth arr fr)
        arr2 (assoc arr fr (nth arr to))
        arr3 (assoc arr2 to cur)]
    (str/join arr3)))


;; swap position 0 with position 2
(defn parse-swap-position
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr to] (re-find (re-matcher #"^swap position (\d+) with position (\d+)$" cmd))]
    (list swap-position text (parse-int fr) (parse-int to))))

;; reverse positions X through Y means that the span of letters at
;; indexes X through Y (including the letters at X and Y) should be
;; reversed in order.
(defn reverse-positions
  [text fr to]
  (let [arr (vec (seq text))]
    (str/join (concat (subvec arr 0 fr)
                      (reverse (subvec arr fr (inc to)))
                      (subvec arr (inc to))))))



;; reverse positions 1 through 6
(defn parse-reverse-positions
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr to] (re-find (re-matcher #"^reverse positions (\d+) through (\d+)$" cmd))]
    (list reverse-positions text (parse-int fr) (parse-int to))))


;; rotate left/right X steps means that the whole string should be
;; rotated; for example, one right rotation would turn abcd into
;; dabc.
(defn rotate-right
  [text fr]
  (let [arr (seq text)
        fr (mod fr (count arr))
        cnt (- (count arr) fr)]
    ;; 12345 2 => 45123
    (str/join (concat (nthrest arr cnt)(take cnt arr)))))


;; rotate right 4 steps
(defn parse-rotate-right
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr] (re-find (re-matcher #"^rotate right (\d+) steps?$" cmd))]
    (list rotate-right text (parse-int fr))))

;; rotate left/right X steps means that the whole string should be
;; rotated; for example, one right rotation would turn abcd into
;; dabc.
(defn rotate-left
  [text fr]
  (let [arr (vec (seq text))
        fr (mod fr (count arr))]
    (str/join (concat (nthrest arr fr)(take fr arr)))))


;; rotate right 4 steps
(defn parse-rotate-left
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr] (re-find (re-matcher #"^rotate left (\d+) steps?$" cmd))]
    (list rotate-left text (parse-int fr))))


(defn rotate-based-on
  [text fr]
  (if-let [idx (str/index-of text fr)]
    (rotate-right text (inc (if (>= idx 4)(inc idx) idx)))))



;; rotate based on position of letter X means that the whole string
;; should be rotated to the right based on the index of letter X
;; (counting from 0) as determined before this instruction does any
;; rotations. Once the index is determined, rotate the string to the
;; right one time, plus a number of times equal to that index, plus
;; one additional time if the index was at least 4.

;; (rotate-based-on "ecabd" "d") => "decab"
;; (rotate-based-on "abdec" "b") => "ecabd"

;; rotate based on position of letter a
(defn parse-rotate-based-on
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr] (re-find (re-matcher #"^rotate based on position of letter ([a-z]+)$" cmd))]
    (list rotate-based-on text fr)))


;; swap letter X with letter Y means that the letters X and Y should
;; be swapped (regardless of where they appear in the string).
(defn swap-letter
  [text fr to]
  (-> text
   (str/replace fr ".")
   (str/replace to fr)
   (str/replace "." to)))



;; swap letter d with letter a
(defn parse-swap-letter
  "Apply cmd to text. Return text as is if cmd isn't this cmd"
  [cmd text]
  (if-let [[_ fr to] (re-find (re-matcher #"^swap letter ([a-z]+) with letter ([a-z]+)$" cmd))]
    (list swap-letter text fr to)))

(defn mycall
  "form is a list where first arg is a function and rest is args. Execute it if non-nil.
   Not using eval, since eval is much more general, like handles macros etc."
  [form]
  (if form (apply (first form)(rest form))))

(defn parse-cmd
  [cmd text]
  (let [ret (or (parse-rotate-left cmd text)
                (parse-rotate-right cmd text)
                (parse-move-position cmd text)
                (parse-reverse-positions cmd text)
                (parse-rotate-based-on cmd text)
                (parse-swap-letter cmd text)
                (parse-swap-position cmd text))]
    (if (nil? ret){:error :not-a-command :data cmd}
        ret)))


;; testdata defined further down
(declare mydata)

(defn process-2
  [text l]
  (if (seq l)
    (do
      (assert (first l))
      (let [res (mycall (parse-cmd (first l) text))]
        (println (str (first l) " => " res))
        (recur res (rest l))))
    text))

(defn process
  ([text](process text (str/split-lines mydata)))
  ([text l](process-2 text l)))


(def testdata
  "swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d
")

(defn sample
  []
  (process "abcde" (str/split-lines testdata)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;; (call-cmd-backwards "abc" (list rotate-right "foo" 1) "cab") ==> true
;; overkill except for non-reversible operations
;; * move-position
;; * rotate-based-on
(defn call-cmd-backwards
  "Return non-nil if text is the correct answer."
  [text cmd text-after]
  (= text-after
     (mycall (cons (first cmd) (cons text (rest (rest cmd)))))))

(defn try-call-cmd-backwards
  ([cmd text-after](try-call-cmd-backwards (combo/permutations (seq text-after)) (parse-cmd cmd "dummy") text-after))
  ([perm parsed-cmd text-after]
   (if-not (seq? perm) nil
           (let [text (str/join (first perm))
                 found (call-cmd-backwards text parsed-cmd text-after)]
             (if found text
                 (recur (rest perm) parsed-cmd text-after))))))

(defn process-b-2
  [text-after l]
  (if (seq l)
    (do
      (assert (first l))
      (let [res (try-call-cmd-backwards (first l) text-after)]
        (println (str (first l) " => " res))
        (recur res (rest l))))
    text-after))

;; (mw.utils/timed "foo" (process-b "fbgdceah")) => "gcehdbfa"
;; (mw.utils/timed "foo" (process "gcehdbfa")) => "fbgdceah"
(defn process-b
  ([text-after](process-b text-after (reverse (str/split-lines mydata))))
  ([text-after l](process-b-2 text-after l)))


(defn sample-b
  []
  (assert
   (= "abcde"
      (process-b "decab" (reverse (str/split-lines testdata))))))



(def mydata
  "move position 2 to position 1
move position 2 to position 5
move position 2 to position 4
swap position 0 with position 2
move position 6 to position 5
swap position 0 with position 4
reverse positions 1 through 6
move position 7 to position 2
rotate right 4 steps
rotate left 6 steps
rotate based on position of letter a
rotate based on position of letter c
move position 2 to position 0
swap letter d with letter a
swap letter g with letter a
rotate left 6 steps
reverse positions 4 through 7
swap position 6 with position 5
swap letter b with letter a
rotate based on position of letter d
rotate right 6 steps
move position 3 to position 1
swap letter g with letter a
swap position 3 with position 6
rotate left 7 steps
swap letter b with letter c
swap position 3 with position 7
move position 2 to position 6
swap letter b with letter a
rotate based on position of letter d
swap letter f with letter b
move position 3 to position 4
rotate left 3 steps
rotate left 6 steps
rotate based on position of letter c
move position 1 to position 3
swap letter e with letter a
swap letter a with letter c
rotate left 2 steps
move position 6 to position 5
swap letter a with letter g
rotate left 5 steps
reverse positions 3 through 6
move position 7 to position 2
swap position 6 with position 5
swap letter e with letter c
reverse positions 2 through 7
rotate based on position of letter e
swap position 3 with position 5
swap letter e with letter d
rotate left 3 steps
rotate based on position of letter c
move position 4 to position 7
rotate based on position of letter e
reverse positions 3 through 5
rotate based on position of letter h
swap position 3 with position 0
swap position 3 with position 4
move position 7 to position 4
rotate based on position of letter a
reverse positions 6 through 7
rotate based on position of letter g
swap letter d with letter h
reverse positions 0 through 3
rotate right 2 steps
rotate right 6 steps
swap letter a with letter g
reverse positions 2 through 4
rotate based on position of letter e
move position 6 to position 0
reverse positions 0 through 6
move position 5 to position 1
swap position 5 with position 2
rotate right 3 steps
move position 3 to position 1
rotate left 1 step
reverse positions 1 through 3
rotate left 4 steps
reverse positions 5 through 6
rotate right 7 steps
reverse positions 0 through 2
move position 0 to position 2
swap letter b with letter c
rotate based on position of letter d
rotate left 1 step
swap position 2 with position 1
swap position 6 with position 5
swap position 5 with position 0
swap letter a with letter c
move position 7 to position 3
move position 6 to position 7
rotate based on position of letter h
move position 3 to position 0
move position 4 to position 5
rotate left 4 steps
swap letter h with letter c
swap letter f with letter e
swap position 1 with position 3
swap letter e with letter b
rotate based on position of letter e
")

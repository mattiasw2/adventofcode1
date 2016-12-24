(ns adventofcode1.twentythree
  "http://adventofcode.com/2016/day/23"
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
   [com.rpl.specter :as sp]

   spyscope.core
   mw.utils
   adventofcode1.spec-test-instrument-debug))

;; specter ideas: for example how to go to any eelement
;; https://github.com/nathanmarz/specter/issues/57

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computer definition

;; Initially, only "a" to "d"
(s/def ::regs (s/map-of string? integer?))
(s/def ::ip   integer?)

(s/def ::computer (s/keys :req-un [::ip ::regs]))

;; since :y doesn't always exists, I might have to remove it
(s/def ::cmd  (s/keys :reg-un [::cmd ::x ::y]))
(s/def ::cmds (s/coll-of ::cmd))



(s/fdef init-computer
        :args (s/cat)
        :ret  ::computer)

(defn init-computer
  []
  {:ip 0
   :regs {"a" 0 "b" 0 "c" 0 "d" 0}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(defn parse-int
  [s]
  (int (bigint s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse instructions

(defn parse-cpy-value
  [cmd]
  (if-let [[_ x y] (re-find (re-matcher #"^cpy (-?\d+) ([a-z]+)$" cmd))]
    {:cmd :cpy-value :x (parse-int x) :y y}))

(defn parse-cpy-reg
  [cmd]
  (if-let [[_ x y] (re-find (re-matcher #"^cpy ([a-z]+) ([a-z]+)$" cmd))]
    {:cmd :cpy-reg :x x :y y}))

(defn parse-inc
  [cmd]
  (if-let [[_ x] (re-find (re-matcher #"^inc ([a-z]+)$" cmd))]
    {:cmd :inc :x x}))

(defn parse-dec
  [cmd]
  (if-let [[_ x] (re-find (re-matcher #"^dec ([a-z]+)$" cmd))]
    {:cmd :dec :x x}))

;; does this instruction exists? yes!
(defn parse-jnz-value
  [cmd]
  (if-let [[_ x y] (re-find (re-matcher #"^jnz (-?\d+) (-?\d+)$" cmd))]
    {:cmd :jnz-value :x (parse-int x) :y (parse-int y)}))

(defn parse-jnz-value-2
  [cmd]
  (if-let [[_ x y] (re-find (re-matcher #"^jnz (-?\d+) ([a-z]+)$" cmd))]
    {:cmd :jnz-value :x (parse-int x) :y y}))

;; do we know the 2nd arg is always a number? Yes, at least in my sample
(defn parse-jnz-x-reg
  [cmd]
  (if-let [[_ x y] (re-find (re-matcher #"^jnz ([a-z]+) (-?\d+)$" cmd))]
    {:cmd :jnz-x-reg :x x :y (parse-int y)}))

(defn parse-tgl
  [cmd]
  (if-let [[_ x] (re-find (re-matcher #"^tgl ([a-z]+)$" cmd))]
    {:cmd :tgl :x x}))



(defn parse-cmd
  [cmd]
  (let [ret (or (parse-cpy-value cmd)
                (parse-cpy-reg cmd)
                (parse-inc cmd)
                (parse-dec cmd)
                (parse-jnz-value cmd)
                (parse-jnz-value-2 cmd)
                (parse-jnz-x-reg cmd)
                (parse-tgl cmd))]
    (if (nil? ret)(assert false {:error :not-a-command :data cmd})
        ret)))

;; testdata defined further down
(declare sample-data)
(declare sample-data-2nd)
(declare puzzle-input-a)
(declare puzzle-input-b)

(defn parse-cmds
  ([](parse-cmds (str/split-lines puzzle-input-a)))
  ([cmds]
   (vec (map parse-cmd cmds))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; execution

(s/fdef tgl
        :args (s/cat :cmds ::cmds :cmd ::cmd :ip ::ip :regs ::regs)
        :ret  (s/tuple ::cmds ::ip ::regs))

(defn tgl
  [cmds cmd ip regs]
  (let [idx (+ ip (get regs (:x cmd)))]
    (if (or (neg? idx) (>= idx (count cmds)))
      [cmds (inc ip) regs]
      (let
          [before (nth cmds idx)
           after
           (match [(:cmd before)(:y before)]
             [:inc nil] :dec
             [_    nil] :inc
             [:jnz-x-reg _] :cpy-reg
             [:jnz-value _] :cpy-value
             [:cpy-reg _]   :jnz-x-reg
             [:cpy-value _] :jnz-value)
           cmds2 (assoc cmds idx (sp/setval [:cmd] after before))]
        [cmds2 (inc ip) regs]))))

(s/fdef maybe-deref
        :args (s/cat :regs ::regs :key (s/or :value integer? :reg string?))
        :ret  integer?)

(defn maybe-deref
  [regs s]
  (if (integer? s)
    s
    (let [res (get regs s)
          _ (assert (not (nil? res)))]
      res)))

(s/fdef exec
        :args (s/cat :cmds ::cmds :cmd ::cmd :ip ::ip :regs ::regs)
        :ret  (s/tuple ::cmds ::ip ::regs))

(defn exec
  [cmds cmd ip regs]
  (match (:cmd cmd)
    :cpy-value [cmds (inc ip) (assoc regs (:y cmd) (:x cmd))]
    :cpy-reg   [cmds (inc ip) (assoc regs (:y cmd) (get regs (:x cmd)))]
    :inc       [cmds (inc ip) (assoc regs (:x cmd) (inc (get regs (:x cmd))))]
    :dec       [cmds (inc ip) (assoc regs (:x cmd) (dec (get regs (:x cmd))))]
    :jnz-value (if-not (zero? (:x cmd))
                 [cmds
                  (+ ip (maybe-deref regs (:y cmd)))
                  regs]
                 [cmds (inc ip) regs])
    :jnz-x-reg (if-not (zero? (get regs (:x cmd)))
                 [cmds (+ ip (:y cmd)) regs]
                 [cmds (inc ip) regs])
    :tgl       (tgl cmds cmd ip regs)))


(defn main
  ;; ([](main (parse-cmds (str/split-lines sample-data-2nd)) (init-computer)))
  ;;([](main (parse-cmds (str/split-lines sample-data)) (init-computer)))
  ;; first puzzle
  ;;([](main (parse-cmds (str/split-lines puzzle-input-a)) (init-computer)))
  ;; 2nd puzzle
  ([](main 0 (parse-cmds (str/split-lines puzzle-input-b))
           {:ip 0 :regs {"a" 7 "b" 0 "c" 0 "d" 0}}))
  ([ctr cmds comp]
   (if (zero? (mod ctr 100000)) (println (str ctr ": " comp)))
   (let [{:keys [ip regs]} comp]
     ;; we are finished if there is no more cmds to run
     (if (or (neg? ip)(>= ip (count cmds)))
         comp
         (let [[cmds2 ip2 regs2] (exec cmds (nth cmds ip) ip regs)]
           (recur (inc ctr) cmds2 {:ip ip2 :regs regs2}))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample data

(def sample-data
  "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a")

(def sample-data-2nd
   "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a")

(def puzzle-input-a
  "cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 14 c
cpy 14 d
inc a
dec d
jnz d -2
dec c
jnz c -5")

(def puzzle-input-b
  "cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2
tgl c
cpy -16 c
jnz 1 c
cpy 99 c
jnz 77 d
inc a
inc d
jnz d -2
inc c
jnz c -5")

(clojure.spec.test/instrument)

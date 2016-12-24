(ns adventofcode1.twelf
  "http://adventofcode.com/2016/day/12"
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

;; do we know the 2nd arg is always a number? Yes, at least in my sample
(defn parse-jnz-x-reg
  [cmd]
  (if-let [[_ x y] (re-find (re-matcher #"^jnz ([a-z]+) (-?\d+)$" cmd))]
    {:cmd :jnz-x-reg :x x :y (parse-int y)}))


(defn parse-cmd
  [cmd]
  (let [ret (or (parse-cpy-value cmd)
                (parse-cpy-reg cmd)
                (parse-inc cmd)
                (parse-dec cmd)
                (parse-jnz-value cmd)
                (parse-jnz-x-reg cmd))]
    (if (nil? ret)(assert false {:error :not-a-command :data cmd})
        ret)))

;; testdata defined further down
(declare sample-data)
(declare puzzle-input-a)

(defn parse-cmds
  ([](parse-cmds (str/split-lines puzzle-input-a)))
  ([cmds]
   (vec (map parse-cmd cmds))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; execution

(s/fdef exec
        :args (s/cat :cmd ::cmd :ip ::ip :regs ::regs)
        :ret  (s/tuple ::ip ::regs))

(defn exec
  [cmd ip regs]
  (match (:cmd cmd)
    :cpy-value [(inc ip) (assoc regs (:y cmd) (:x cmd))]
    :cpy-reg   [(inc ip) (assoc regs (:y cmd) (get regs (:x cmd)))]
    :inc       [(inc ip) (assoc regs (:x cmd) (inc (get regs (:x cmd))))]
    :dec       [(inc ip) (assoc regs (:x cmd) (dec (get regs (:x cmd))))]
    :jnz-value (if-not (zero? (:x cmd))
                 [(+ ip (:y cmd)) regs]
                 [(inc ip) regs])
    :jnz-x-reg (if-not (zero? (get regs (:x cmd)))
                 [(+ ip (:y cmd)) regs]
                 [(inc ip) regs])))



(defn main
  ;;([](main (parse-cmds (str/split-lines sample-data)) (init-computer)))
  ;; first puzzle
  ;;([](main (parse-cmds (str/split-lines puzzle-input-a)) (init-computer)))
  ;; 2nd puzzle
  ([](main (parse-cmds (cons "cpy 1 c" (str/split-lines puzzle-input-a)))
           (init-computer)))
  ([cmds comp]
   (let [{:keys [ip regs]} comp]
     ;; we are finished if there is no more cmds to run
     (if (or (neg? ip)(>= ip (count cmds)))
         comp
         (let [[ip2 regs2] (exec (nth cmds ip) ip regs)]
           (recur cmds {:ip ip2 :regs regs2}))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample data

(def sample-data
  "cpy 41 a
inc a
inc a
dec a
jnz a 2
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


;; (clojure.spec.test/instrument)

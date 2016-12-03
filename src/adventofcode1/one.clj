(ns adventofcode1.one
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]

   [clojure.core.match :refer [match]]

   ;; You can also call (timbre/refer-timbre) to configure Clj ns referrals automatically
   [taoensso.timbre :as timbre
    :refer (log  trace  debug  info  warn  error  fatal  report
                 logf tracef debugf infof warnf errorf fatalf reportf
                 spy get-env log-env)]
   spyscope.core
   )
  )

;; Assume we are inside a world of -max-coord .. (max-coord-1)
;; limited for testing
(def max-coord 100)

(do
  ;;(set! *warn-on-reflection* true)
  ;;(set! *unchecked-math* :warn-on-boxed)
  (s/check-asserts true)
  )

(assert s/*compile-asserts*)
(assert clojure.lang.RT/checkSpecAsserts)
(assert (s/check-asserts?))

;; Tip: ?pos cannot generate, integer? is needed for that part.
;; Tip: and (s/int-in 0 11) is even better if we are talking about small numbers
(s/def ::l (s/and (s/int-in 0 max-coord) integer?  #(< % max-coord)))
;; Tip: you cannot use s/or for #(or (pos? %) (zero? %)) , i.e. (s/or :pos pos? :zero zero?) is not the same
(s/def ::r (s/and (s/int-in 0 max-coord) integer? #(or (pos? %) (zero? %)) #(< % max-coord)))

;; path with name-spaced keys
;; (s/def ::step (s/or :turn-left (s/keys :req    [::l]) :turn-right (s/keys :req    [::r])))
;; (s/def ::path (s/* ::step))
;; (gen/generate (s/gen ::path))

;; path with unqualified keyes
;; s/or and s/alt are very different, since s/alt is a sequence
(s/def ::step-un (s/or :turn-left (s/keys :req-un [::l]) :turn-right (s/keys :req-un [::r])))
;; when a SINGLE arg is a sequence, do not use s/* s/cat or similar, like ::path above
(s/def ::path-un (s/coll-of ::step-un))
;; (gen/generate (s/gen ::path-un))

;; y 2
;; y 1
;; y 0
;; y -1
;; y -2
;; x -2 -1 0 1 2
;;

(s/def ::x (s/int-in (* -1 max-coord) max-coord))
(s/def ::y (s/int-in (* -1 max-coord) max-coord))

(s/def ::xy (s/keys :req-un [::x ::y]))

(s/def ::x-direction (s/int-in -1 2))
(s/def ::y-direction (s/int-in -1 2))

(defn no-diagonals?
  "Return true if the direction is N,S,W,E"
  [direction]
  (= 1 (Math/abs (apply + (vals direction)))))

(s/def ::xy-direction (s/and (s/keys :req-un [::x-direction ::y-direction]) no-diagonals?))

(def north {:x-direction  0 :y-direction  1})
(def south {:x-direction  0 :y-direction -1})
(def east  {:x-direction  1 :y-direction  0})
(def west  {:x-direction -1 :y-direction  0})

(def turn-left
  {north west,
   south east,
   east  north,
   west  south})

(def turn-right
  {north east,
   south west,
   east  south,
   west  north})

;; straight is only a partial function if the board size if fixed, since we might to move outside it
;; (clojure.spec.test/check `straight)
#_(s/fdef straight
        :args (s/cat :current ::xy :direction ::xy :n integer?)
        :ret ::xy)


;; will it still find a counter example, it exists, but can it be found?
;; (clojure.spec.test/check `straight)
;; yes! :smallest [({:x 0, :y 90} {:x-direction 0, :y-direction 1} 10)]}},
(s/fdef straight
        :args (s/cat :current ::xy :direction ::xy-direction :n (s/int-in 0 11))
        :ret ::xy
        ;; just testing :fn
        ;; {:args {:current {:x 0, :y 1}, :direction {:x-direction 0, :y-direction 1}, :n 1}, :ret {:x 0, :y 2}}
        ;; :fn (fn [d] (println d) (= 0 (rand-int 2)))
        )


(defn straight
  "Move n times in direction starting at current, and return new current"
  [current direction n]
  {:x (+ (:x current)(* (:x-direction direction) n)),
   :y (+ (:y current)(* (:y-direction direction) n))}
  )

(s/fdef step
        :args (s/cat :current ::xy :direction ::xy-direction :step ::step-un)
        :ret  (s/tuple ::xy ::xy-direction)
        )

(defn step
  "Move a single step.
   Return new current and direction"
  [current direction step]
  (let [[new-direction n]
        (match step
          {:l n} [(turn-left  direction) n]
          {:r n} [(turn-right direction) n])
        new-current (straight current new-direction n)]
    [new-current new-direction]))

(s/fdef steps
        :args (s/or :base (s/cat :path ::path-un) :rec (s/cat :current ::xy :direction ::xy-direction :path ::path-un))
        ;; :args (s/or :base (s/cat :path ::path-un) :rec (s/cat :current ::xy :direction ::xy-direction :path ::path-un))
        :ret  ::xy
        )

(defn steps
  "Move a number of steps"
  ([path](steps {:x 0 :y 0} north path))
  ([current direction path]
   (if (empty? path) current
       (let [[new-current new-direction] (step current direction (first path))]
         (recur new-current new-direction (rest path))))))


(clojure.spec.test/instrument)

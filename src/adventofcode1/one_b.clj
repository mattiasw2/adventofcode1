(ns adventofcode1.one-b
  "http://adventofcode.com/2016/day/1"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]

   [clojure.core.match :refer [match]]

   spyscope.core
   )
  )

;; Assume we are inside a world of -max-coord .. (max-coord-1)
;; limited for testing
(def max-coord 1000)

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
;; :c is straight ahead in the current direction
(s/def ::c (s/int-in 0 max-coord))

;; path with name-spaced keys
;; (s/def ::step (s/or :turn-left (s/keys :req    [::l]) :turn-right (s/keys :req    [::r])))
;; (s/def ::path (s/* ::step))
;; (gen/generate (s/gen ::path))

;; path with unqualified keyes
;; s/or and s/alt are very different, since s/alt is a sequence
(s/def ::step-un (s/or :continue (s/keys :req-un [::c]) :turn-left (s/keys :req-un [::l]) :turn-right (s/keys :req-un [::r])))
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

(defn distance
  [current-or-direction]
  (apply + (map #(Math/abs %) (vals current-or-direction))))

(defn no-diagonals?
  "Return true if the direction is N,S,W,E"
  [direction]
  (= 1 (distance direction)))

(s/def ::xy-direction (s/and (s/keys :req-un [::x-direction ::y-direction]) no-diagonals?))

;; moved here, since no-diagonals? used in s/def above
;; this are just for learning specs better
(s/fdef distance
        :args (s/alt :current ::xy :direction ::xy-direction)
        :ret  integer?
        )

(s/fdef no-diagonals?
        :args (s/cat :direction ::xy-direction)
        :ret  boolean?
        )


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

(def start {:x 0 :y 0})

;; straight is only a partial function if the board size if fixed, since we might to move outside it
;; (clojure.spec.test/check `straight)
#_(s/fdef straight
        :args (s/cat :current ::xy :direction ::xy :n integer?)
        :ret ::xy)


;; will it still find a counter example, it exists, but can it be found?
;; (clojure.spec.test/check `straight)
;; yes! :smallest [({:x 0, :y 90} {:x-direction 0, :y-direction 1} 10)]}},
(s/fdef straight
        :args (s/cat :current ::xy :direction ::xy-direction :n (s/int-in 0 max-coord))
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
          {:c n} [direction n]
          {:l n} [(turn-left  direction) n]
          {:r n} [(turn-right direction) n])
        new-current (straight current new-direction n)]
    [new-current new-direction]))

(s/fdef steps
        :args (s/or :base (s/cat :path ::path-un) :rec (s/cat :current ::xy :direction ::xy-direction :path ::path-un))
        :ret  ::xy
        )

(defn steps
  "Move a number of steps"
  ([path](steps start north path))
  ([current direction path]
   (if (empty? path) current
       (let [[new-current new-direction] (step current direction (first path))]
         (recur new-current new-direction (rest path))))))

;; todo: we should probably have 2 definitions for samples like this, one with s/int-in, and ione without
;; because, now things like (stest/check `adventofcode1.one/total-distance) will always fail
(s/fdef total-distance
        :args (s/cat :path ::path-un)
        :ret  integer?
        )

(defn total-distance
  "Calculate the nearest way to the final current position"
  [path]
  (distance (steps path)))


(defn core-path
  "Make a core path from a path. In a core path, the steps are always 0 or 1.
   :c is used for this, for example R3 is replaced by R1 C1 C1.
   The only time a 0 is generated if input is L0 or R0, since then we would find the bunny directly."
  ([path](core-path [] (first path)(rest path)))
  ([sofar hd tl]
   (match hd
     nil    sofar
     {:r 0} (recur (conj sofar hd    ) (first tl)  (rest tl))
     {:l 0} (recur (conj sofar hd    ) (first tl)  (rest tl))
     {:c 0} (recur (conj sofar hd    ) (first tl)  (rest tl))
     {:r 1} (recur (conj sofar {:r 1}) (first tl)  (rest tl))
     {:l 1} (recur (conj sofar {:l 1}) (first tl)  (rest tl))
     {:c 1} (recur (conj sofar {:c 1}) (first tl)  (rest tl))
     {:r n} (recur (conj sofar {:r 1}) {:c (dec n)} tl      )
     {:l n} (recur (conj sofar {:l 1}) {:c (dec n)} tl      )
     {:c n} (recur (conj sofar {:c 1}) {:c (dec n)} tl      )
     )))


(defn stop-same-position
  "Move a number of steps and exit when we STOP at the same cell the second time, or when the path is empty.
   This wasn't what they wanted."
  ([path](stop-same-position start north #{start} (core-path path)))
  ([current direction passed path]
   (if (empty? path) "No easter bunny found"
       (let [[new-current new-direction] (step current direction (first path))]
         (if (passed new-current)
           (distance new-current)
           (recur new-current new-direction (conj passed new-current)(rest path)))))))

(clojure.spec.test/instrument)

(ns adventofcode1.two
  "http://adventofcode.com/2016/day/2"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]

   [clojure.core.match :refer [match]]

   spyscope.core
   )
  )

(s/def ::key #{:1 :2 :3 :4 :5 :6 :7 :8 :9})

(def up
  {:1 nil, :2 nil, :3 nil
   :4 :1 , :5 :2 , :6 :3
   :7 :4 , :8 :5 , :9 :6
   })

(def down
  {:1 :4 , :2 :5 , :3 :6
   :4 :7 , :5 :8 , :6 :9
   :7 nil, :8 nil, :9 nil
   })

(def left
  {:1 nil, :2 :1 , :3 :2
   :4 nil, :5 :4 , :6 :5
   :7 nil, :8 :7 , :9 :8
   })

(def right
  {:1 :2 , :2 :3 , :3 nil
   :4 :5 , :5 :6 , :6 nil
   :7 :8 , :8 :9 , :9 nil
   })

(s/def ::move #{\U \D \L \R})

(s/fdef move-or-ignore
        :args (s/cat :current ::key :move ::move)
        :ret ::key)

(defn move-or-ignore
  "Move if possible, otherwise stay at key"
  [current move]
  (or (current (match move
                 \U up
                 \D down
                 \R right
                 \L left))
      ;; if nil above, stay at same key
      current))

(s/fdef row-2
        :args (s/cat :current ::key :moves (s/coll-of ::move))
        :ret ::key)

(defn row-2
  [current moves]
  (if (empty? moves) current
      (recur (move-or-ignore current (first moves))
             (rest moves))))


(s/fdef row
        :args (s/cat :current ::key :moves string?)
        :ret ::key)

(defn row
  "Find the value for one row of moves"
  [current moves]
  (row-2 current (seq moves)))

(defn rows
  "Start at button 5 and process moves row by row."
  ([list-of-moves](rows [] :5 list-of-moves))
  ([answer current list-of-moves]
   (if (empty? list-of-moves) answer
       (let [new-current (row current (first list-of-moves))]
         (recur (conj answer new-current) new-current (rest list-of-moves))))))


(clojure.spec.test/instrument)

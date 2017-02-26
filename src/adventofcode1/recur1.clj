(ns adventofcode1.recur1
  "tail-recursion"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]

   [clojure.core.match :refer [match]]

   spyscope.core
   )
  )

(def function-table (zipmap '(+ - * pd)
                            '(2 2 2 2)))

(defn random-function
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  []
  (rand-nth (list 'x (- (rand 10) 5))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          )
    (random-terminal)
    (let [f (random-function)]
      (cons f (doall (repeatedly (get function-table f)
                                 #(random-code (dec depth))))))))

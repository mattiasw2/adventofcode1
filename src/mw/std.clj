(ns mw.std
  (:require
    [schema.core :as s]
    [taoensso.truss :as truss :refer (have have! have?)]
    [taoensso.timbre :as timbre]
    )
  (:gen-class)
  )

;; this error monad is cool too: https://brehaut.net/blog/2011/error_monads
(defmacro try*
  [& body]
  `(try [:ok (do ~@body)] (catch Exception e# [:error e#])))

;; just as mapv, i.e. non-lazy and return vector
(defmacro forv [& body]
  `(vec (for ~@body)))

(defn fetch
  "(fetch :foo x) == (:foo x) but aborts if result is nil"
  [k v]
  (let [res (get v k)]
    ;; (some? is the same as not-nil?
    (assert (some? res) (str k " not found in " v))
    res))

(defn nn
  "not-nil: abort if value nil"
  [v]
  (assert (some? v))
  v)

(defmacro def- [item value]
  `(def ^{:private true} ~item ~value)
  )

;; works well
(defmacro loading []
  `(do (taoensso.timbre/info ~(str "loading " *file* " "  (:line (meta &form))))
       (set! *warn-on-reflection* true)
       (schema.core/set-fn-validation! true)))


(defmacro ignore-exception [& form]
  `(try ~@form (catch Exception exc# :ignored)))


;; (defmacro my-println [x]
;;   `(do (printf "%s:%s> %s is %s\n"
;;                ~*file*
;;                ~(:line (meta &form))
;;                ~(pr-str x)
;;                ~x)
;;        (flush)))

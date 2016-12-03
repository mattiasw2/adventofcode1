;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns adventofcode1.spec-test-instrument-debug
  (:refer-clojure :exclude [test])
  (:require
   [clojure.pprint :as pp]
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.string :as str]))

(in-ns 'clojure.spec.test.check)
(in-ns 'clojure.spec.test)
(alias 'stc 'clojure.spec.test.check)

(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        conform! (fn [v role spec data args]
                   (let [conformed (s/conform spec data)]
                     (if (= ::s/invalid conformed)
                       (let [caller (->> (.getStackTrace (Thread/currentThread))
                                         stacktrace-relevant-to-instrument
                                         first)
                             ed (merge (assoc (s/explain-data* spec [role] [] [] data)
                                         ::s/args args
                                         ::s/failure :instrument)
                                       (when caller
                                         {::caller (dissoc caller :class :method)}))]
                         (throw (ex-info
                                 (str "Call to " v " did not conform to spec:\n" (with-out-str (s/explain-out ed)))
                                 ed)))
                       conformed)))]
    (fn
     [& args]
     (if *instrument-enabled*
       (with-instrument-disabled
         (let [specs fn-spec]
           (let [cargs (when (:args specs) (conform! v :args (:args specs) args args))
                 ret (binding [*instrument-enabled* true]
                       (.applyTo ^clojure.lang.IFn f args))
                 cret (when (:ret specs) (conform! v :ret (:ret specs) ret args))]
             (when (and (:args specs) (:ret specs) (:fn specs))
               (conform! v :fn (:fn specs) {:args cargs :ret cret} args))
             ret)))
       (.applyTo ^clojure.lang.IFn f args)))))

;; old definition; https://github.com/clojure/clojure/blob/0bc837b9c25ae62185795b2bf2c7952bf6e12d9e/src/clj/clojure/spec.clj
#_(defn- spec-checking-fn
  [v f]
  (let [conform! (fn [v role spec data args]
                   (let [conformed (conform spec data)]
                     (if (= ::invalid conformed)
                       (let [ed (assoc (explain-data* spec [role] [] [] data)
                                  ::args args)]
                         (throw (ex-info
                                 (str "Call to " v " did not conform to spec:\n" (with-out-str (explain-out ed)))
                                 ed)))
                       conformed)))]
    (c/fn
     [& args]
     (if *instrument-enabled*
       (with-instrument-disabled
         (let [specs (fn-specs v)]
           (let [cargs (when (:args specs) (conform! v :args (:args specs) args args))
                 ret (binding [*instrument-enabled* true]
                       (.applyTo ^clojure.lang.IFn f args))
                 cret (when (:ret specs) (conform! v :ret (:ret specs) ret args))]
             (when (c/and (:args specs) (:ret specs) (:fn specs))
               (conform! v :fn (:fn specs) {:args cargs :ret cret} args))
             ret)))
       (.applyTo ^clojure.lang.IFn f args)))))

;; 1.9-alpha14 version
#_(defn- spec-checking-fn
  [v f fn-spec]
  (let [fn-spec (@#'s/maybe-spec fn-spec)
        conform! (fn [v role spec data args]
                   (let [conformed (s/conform spec data)]
                     (if (= ::s/invalid conformed)
                       (let [caller (->> (.getStackTrace (Thread/currentThread))
                                         stacktrace-relevant-to-instrument
                                         first)
                             ed (merge (assoc (s/explain-data* spec [role] [] [] data)
                                         ::s/args args
                                         ::s/failure :instrument)
                                       (when caller
                                         {::caller (dissoc caller :class :method)}))]
                         (throw (ex-info
                                 (str "Call to " v " did not conform to spec:\n" (with-out-str (s/explain-out ed)))
                                 ed)))
                       conformed)))]
    (fn
     [& args]
     (if *instrument-enabled*
       (with-instrument-disabled
         (when (:args fn-spec) (conform! v :args (:args fn-spec) args args))
         (binding [*instrument-enabled* true]
           (.applyTo ^clojure.lang.IFn f args)))
       (.applyTo ^clojure.lang.IFn f args)))))

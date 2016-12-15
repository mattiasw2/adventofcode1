(defproject adventofcode1 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 ;; https://github.com/ptaoussanis/timbre
                 [com.taoensso/timbre "4.7.4"]
                 ;; (refresh) seems to work better if I switch to user first: (in-ns 'user)
                 ;; user=> (require '[clojure.tools.namespace.repl :refer [refresh]])
                 ;; user=> (refresh)
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [prismatic/schema "1.1.3"]

                 ;;; frm .lein: spyscope: #spy/p #spy/d #spy/t
                 ;;; (take 20 (repeat #spy/p (+ 1 2 3)))
                 [spyscope "0.1.6"]

                 ;;; quickcheck
                 [com.gfredericks/test.chuck "0.2.7"]

                 ;;; base64 for md5
                 [org.clojure/data.codec "0.1.0"]

                 ;;; prolog and clpfd
                 [org.clojure/core.logic "0.8.11"]

                 ]
  :main ^:skip-aot adventofcode1.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  )

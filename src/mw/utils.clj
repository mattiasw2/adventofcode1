(ns mw.utils
  (:require
   [taoensso.truss :as truss :refer (have have! have?)]
   [mw.std :refer :all]
   )
  (:gen-class)
  )

;;; with in clojure is a good idea if you do like the Erlang/Elixir
;;; way of returning tuples like [:ok return-value]
;;; http://blog.trenpixster.info/a-saner-way-to-deal-with-complex-flows/

;;; contains? doesn't do this on sequences.
(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn record->map-2
  "Convert record to map recursively, so that it can be compared to a map.
   Typical used in unit tests"
  [rec]
  (into {} (for [[k v] rec]
             (if (or (record? v)(map? v)) [k (record->map-2 v)]
                 [k v]))))

(defn record->map
  "Convert record to map recursively, so that it can be compared to a map.
   Typical used in unit tests"
  [rec]
  (assert (record? rec))
  (record->map-2 rec))

(defn record->map-2-seqs-too
  "Convert record to map recursively, so that it can be compared to a map.
   Typical used in unit tests

   Will also vectors and seqs contains records"
  [rec]
  (into {} (for [[k v] rec]
             (if (or (record? v)(map? v)) [k (record->map-2-seqs-too v)]
                 (if (vector? v) [k (mapv record->map-2-seqs-too v)]
                     (if (seq? v) [k (map record->map-2-seqs-too v)]
                         [k v]))))))

(defn record->map-seqs-too
  "Convert record to map recursively, so that it can be compared to a map.
   Typical used in unit tests

   Will also vectors and seqs contains records"
  [rec]
  (assert (record? rec))
  (record->map-2-seqs-too rec))

(defn record->map-skip-nil-2
  "Convert record to map recursively, so that it can be compared to a map.
   Typical used in unit tests"
  [rec]
  (into {} (filter
            second ;; same as (fn [kv] (second kv))
            (for [[k v] rec]
              (do ;;(println (str "k: " k ", v: " v))
                  (if (or (record? v)(map? v)) [k (record->map-skip-nil-2 v)]
                      (if (vector? v) [k v] ;; todo: need to recurse differently (mapv record->map-skip-nil-2 v)]
                          (if (seq? v) [k v] ;; todo: need to recurse differently (map record->map-skip-nil-2 v)]
                              [k v]))))))))

(defn record->map-skip-nil
  "Convert record to map recursively, so that it can be compared to a map.
   Typical used in unit tests"
  [rec]
  (assert (record? rec))
  (record->map-skip-nil-2 rec))

(defn into-map-unique
  "Like into, but make sure only new keys are added"
  [to from]
  (reduce
   (fn [coll kv]                        ;[coll [k v]] wasn't working in compiled mode!
     (let [k (first kv)
           v (second kv)]
       (if (find coll k)
         (throw (Exception. (str "key-not-unique " k))))
       (assoc coll k v)))
   to from))

(defn into-debug
  "Works as into, except I will know which field failed"
  [m pairs]
  (if (empty? pairs) m
      (do
        ;;(println (prn-str (first pairs)))
        (let [tmp
              (try (conj m (first pairs))
                   (catch Exception e (println (str "*** failed to insert" pairs))))]
          (recur tmp (rest pairs))))))

;;; didn't work, got compilation problems when refering to RecordSchema, I had to invent (def anyway
;; (defmacro constant
;;   "Only call exp once at compilation of defn. Should be the same as introducing additional (def"
;;   [exp]
;;   (eval exp))

(defmacro timed
  "Just like time, but also prints the top-level function called"
  [txt expr]
  (let [sym (= (type expr) clojure.lang.Symbol)]
    `(let [start# (. System (nanoTime))
           return# ~expr
           res# (if ~sym
                    (resolve '~expr)
                    (resolve (first '~expr)))]
       (prn (str "Timed: " ~txt " "
           (:name (meta res#))
           ": " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
       return#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; byte[]

(defn spit-bytes
  "Like spit, but for byte[]"
  [^String filename ^bytes byts]
  (with-open [w (clojure.java.io/output-stream filename)]
    (.write w byts)))


;;; a really slow implementation, but since only used for unit test, I do not care
;;; (mutils/timed (mutils/byte-array-= (:xml form1) (:xml form1)))
;;; "Timed byte-array-=: 0.839513 msecs"
;;; where byte[] is 4400 bytes long
(defn byte-array-=
  "compare two byte[] for identity, since clojure doesn't seem to compare byte[] by value"
  [x y]
  (let [xcnt (count x)
        ycnt (count y)]
    (or (= 0 xcnt ycnt)
        (and (= xcnt ycnt)
             (reduce (fn [a b] (and a b)) (map #'= x y)))))) ;; (fn [a b] (and a b)) cannot be replaced by and, since macro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helpers to byte byte[] in test code
;;; from http://stackoverflow.com/questions/10062967/clojures-equivalent-to-pythons-encodehex-and-decodehex

(defn hexify "Convert byte sequence to hex string" [coll]
  (let [hex [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]]
      (letfn [(hexify-byte [b]
        (let [v (bit-and b 0xFF)]
          [(hex (bit-shift-right v 4)) (hex (bit-and v 0x0F))]))]
        (clojure.string/join (mapcat hexify-byte coll)))))

(defn hexify-str [s]
  (hexify (.getBytes ^String s)))

(defn unhexify2 "Convert hex string to byte sequence" [s]
      (letfn [(unhexify-2 [c1 c2]
                 (unchecked-byte
                   (+ (bit-shift-left (Character/digit ^char c1 16) 4)
                      (Character/digit ^char c2 16))))]
     (map #(apply unhexify-2 %) (partition 2 s))))

(defn unhexify "Convert hex string to byte[]" [s]
     (byte-array (unhexify2 s)))

(defn unhexify-str [s]
  (clojure.string/join (map char (unhexify2 s))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; call stuff
;;;


;;; good example on how to have optional named args
(defn wait-for
  "Invoke predicate every interval (default 10) seconds until it returns true,
  or timeout (default 150) seconds have elapsed. E.g.:

      (wait-for #(< (rand) 0.2) :interval 1 :timeout 10)

  Returns nil if the timeout elapses before the predicate becomes true, otherwise
  the value of the predicate on its last evaluation."
  [predicate & {:keys [interval timeout]
                :or {interval 10
                     timeout 150}}]
  (let [end-time (+ (System/currentTimeMillis) (* timeout 1000))]
    (loop []
      (if-let [result (predicate)]
        result
        (do
          (Thread/sleep (* interval 1000))
          (if (< (System/currentTimeMillis) end-time)
            (recur)))))))

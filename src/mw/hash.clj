(ns mw.hash
  (:require
   [schema.core :as s]
    [taoensso.truss :as truss :refer (have have! have?)]
   [mw.std :refer :all]
   [clojure.data.codec.base64 :as b64]
;;   [clojure.java.io :as io]
   )
  (:gen-class)
  )

(loading)


(defn hash-string
  "Create MD5 from string, 16 byte, is 32 chars in hex"
  [^String string algo]
  (let [hashed
        (doto (java.security.MessageDigest/getInstance algo)
          (.reset)
          (.update (.getBytes string)))]
    (format "%032x" (new java.math.BigInteger 1 (.digest hashed)))))

(defn hash-string-byte-array
  "Create MD5 from string, 16 byte, is 32 chars in hex"
  [^String string]
  (let [hashed
        (doto (java.security.MessageDigest/getInstance "MD5")
          (.reset)
          (.update (.getBytes string)))]
    (.digest hashed)))


(defn hash-md5
  "Generate a md5 checksum for the given string"
  [string]
  (hash-string string "MD5")
  )

(defn hash-string-integer
  "Use java interop to flexibly hash strings"
  ([string]
   (hash-string-integer string "MD5"))
  ([^String string algo]
   (let [hashed
         (doto (java.security.MessageDigest/getInstance algo)
           (.reset)
           (.update (.getBytes string)))]
     (new java.math.BigInteger 1 (.digest hashed)))))

;;   (m/expt 2 32)
;; 4294967296
;; user>   (m/expt 2 64)
;; 18446744073709551616N
;; user>   (m/expt 2 128)
;; 340282366920938463463374607431768211456N
;; user>   (m/expt 2 256)
;; 115792089237316195423570985008687907853269984665640564039457584007913129639936N
;; user>

(defn hash-b64
  "Create MD5 from string, 16 byte, is 24 chars in base64"
  [^String string]
  (let [hashed
        (doto (java.security.MessageDigest/getInstance "MD5")
          (.reset)
          (.update ^bytes (.getBytes string)))]
    (String. ^bytes (b64/encode
     (.digest hashed)
     ))
    ))

(defn as-b64 [bytes]
  (String. ^bytes (b64/encode bytes)))

(defn b64-as-bytes [str]
  ;; How can BASE64 ever be UTF-8, but since only uses US chars, most likely, not a problem?
  (b64/decode ^bytes (.getBytes ^String str "UTF-8")))


(defn as-hex [bytes]
  (format "%032x" (java.math.BigInteger. 1 ^bytes bytes)))

;;; Why is it difficult to convert byte[] into long?
;;; In C#, you use the BitConverter, but there seems to be no corresponding library in Java
;;; The problem is is UInt64 doesn't exist.
;;;
;;; (require 'mikera.cljutils.bytes)
;;; (def x (hash-string-byte-array "hfdkjhfkjdshkfhfksahfkjsahfkjhdsakfhdsak"))
;;; (long (mikera.cljutils.bytes/slice x 8 8))  => ClassCastException [B cannot be cast to java.lang.Number  clojure.lang.RT.longCast (RT.java:1227)
;;;
;;; Probably, I need to something like this, which of course is complete overkill :-)
;;; http://grepcode.com/file_/repo1.maven.org/maven2/com.expedia.tesla/tesla-core/4.0/com/expedia/tesla/utils/BitConverter.java/?v=source

;; public static long  [More ...] toInt64(byte[] data, int offset) {
;; 	return (((long) (data[offset + 7] & 0xff) << 56)
;; 			| ((long) (data[offset + 6] & 0xff) << 48)
;; 			| ((long) (data[offset + 5] & 0xff) << 40)
;; 			| ((long) (data[offset + 4] & 0xff) << 32)
;; 			| ((long) (data[offset + 3] & 0xff) << 24)
;; 			| ((long) (data[offset + 2] & 0xff) << 16)
;; 			| ((long) (data[offset + 1] & 0xff) << 8) | (data[offset] & 0xff));
;; }

;; public static byte[] getBytes(long v) {
;; 	byte[] writeBuffer = new byte[8];
;; 	writeBuffer[7] = (byte) ((v >>> 56) & 0xFF);
;; 	writeBuffer[6] = (byte) ((v >>> 48) & 0xFF);
;; 	writeBuffer[5] = (byte) ((v >>> 40) & 0xFF);
;; 	writeBuffer[4] = (byte) ((v >>> 32) & 0xFF);
;; 	writeBuffer[3] = (byte) ((v >>> 24) & 0xFF);
;; 	writeBuffer[2] = (byte) ((v >>> 16) & 0xFF);
;; 	writeBuffer[1] = (byte) ((v >>> 8) & 0xFF);
;; 	writeBuffer[0] = (byte) ((v >>> 0) & 0xFF);
;; 	return writeBuffer;
;; }

(ns klj.skip
  "Functions to skip over various kinds of tokens."
  (:require [klj.chars :refer [newline-or-nil?]]
            #?@(:clj
                [[clojure.tools.reader.impl.errors :as err]
                 [clojure.tools.reader.reader-types :refer
                  [peek-char read-char unread]]]
                :cljs
                [[cljs.tools.reader.impl.errors :as err]
                 [clojure.tools.reader.reader-types :refer
                  [peek-char read-char unread]]])))

;; Lifted from clojure.tools.reader.impl.commons
(defn skip-line
  "Advances the reader to the end of a line.
  Supports CR, LF, or CRLF line endings.
  Returns the reader"
  [reader]
  (loop []
    (case (read-char reader)
      (nil \newline) reader
      \return (if (newline-or-nil? (peek-char reader))
                (recur)
                reader)
      (recur))))

(defn skip-to-suffix
  "Skips up to given `suffix`.
  Returns reader, which is positioned just _after_ the suffix."
  [#?(:cljs ^not-native reader :default reader) suffix]
  ;; (println "dmk skip-to-suffix")
  (let [n (count suffix)]
    (loop [ix 0 j 1]
      (if (< ix n)
        (let [c (read-char reader)]
          (cond
            (nil? c) (err/throw-eof-error reader nil)
            (= c (nth suffix ix)) (recur (inc ix) (inc j))
            :else (if (= c (first suffix))
                    (recur 1 (inc j))
                    (recur 0 (inc j)))))
        reader))))

(defn- return-rdr [rdr ch]
  (unread rdr ch)
  rdr)

(defn skip-comment [rdr]
  (when-let [ch (read-char rdr)]
    (case ch
      \# (case (peek-char rdr)
           \space (skip-line rdr)
           \| (skip-to-suffix rdr "|#")
           (return-rdr rdr ch))
      \/ (case (peek-char rdr)
           \/ (skip-line rdr)
           \* (skip-to-suffix rdr "*/")
           (return-rdr rdr ch))
      \; (let [ch2 (peek-char rdr)
               ;TODO: look up matching char
               suffix (case ch2
                        \( ");"
                        \[ "];"
                        \{ "};"
                        \< ">;"
                        \" "\";"
                        \' "';"
                        \| "|;"
                        \! "!;"
                        \* "*;"
                        \# "#;"
                        nil)]
           (if suffix
             (skip-to-suffix rdr suffix)
             (return-rdr rdr ch)))
      (return-rdr rdr ch))))


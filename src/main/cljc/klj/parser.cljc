(ns klj.parser
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.impl.commons :as rc]
            [clojure.tools.reader.impl.errors :as err]
            [clojure.tools.reader.impl.utils :refer [ex-info?]]
            ;TODO: consider using my reader types
            ;I think this is a good candidate for its own small lib
            [clojure.tools.reader.reader-types :refer
             [get-column-number get-file-name get-line-number indexing-reader?
              log-source peek-char read-char unread]]
            [klj.blocks :refer [chained-reader-fn read-delimited-string ?read-comment]]
            [klj.chars :refer [digit? eol-ch? whitespace?]]
            [klj.nodes :refer [as-node bool-node eol-node literal-node nil-node number-node root-node whitespace]]
            [klj.reader :as k])
  (:import (clojure.lang PersistentVector)
           (java.lang Character Exception IllegalStateException Object StringBuilder)))

(def opening-delim? #{\( \{ \[})
(def closing-delim? #{\) \} \]})
(def delim? (set/union opening-delim? closing-delim?))

(defn terminating? [ch]
  (or (nil? ch) (Character/isWhitespace ^Character ch) (closing-delim? ch)))

(defn needs-escape? [ch]
  (case ch
    (\" \( \) \[ \] \{ \}) true
    false))

(defn token-terminating? [ch]
  (or (Character/isWhitespace ^Character ch) (needs-escape? ch)))

(defn read-token
  ^String [reader initch]
  (loop [sb (StringBuilder.) ch initch first? true]
    (cond
      (nil? ch) (str sb)
      (token-terminating? ch) (do (unread reader ch)
                                  (str sb))
      (identical? \\ ch) (recur (doto sb (.append (#'tr/escape-char reader)))
                                (read-char reader)
                                false)
      (or first? (not (#{\: \; \,} ch)))
      (recur (doto sb (.append ch)) (read-char reader) false)
      (terminating? (peek-char reader)) (do (unread reader ch) (str sb))
      (opening-delim? (peek-char reader))
      (throw (IllegalStateException. "Expected EOF, white space or closing delimiter after punctuation at end of token."))
      :else (recur (doto sb (.append ch)) (read-char reader) false))))

(defn parse-space [rdr ch]
  (let [buf (StringBuffer.)]
    (loop [c ch]
      (if (whitespace? c)
        (do (.append buf c) (recur (read-char rdr)))
        (do (unread rdr c) (whitespace (str buf)))))))

(defn parse-separator [_rdr ch]
  (as-node ch))

(defn parse-delimiter [_rdr ch]
  (as-node ch))

(defn parse-eol [rdr ch]
  (loop [c ch eols [] ?return nil]
    (case c
      \newline (recur (read-char rdr) (conj eols (str ?return c)) nil)
      \return (if (= \newline (peek-char rdr))
                (recur (read-char rdr) eols c)
                (recur (read-char rdr) (conj eols "\r") nil))
      (do (unread rdr c) (eol-node eols)))))

(defn parse-string [rdr ch]
  (as-node (read-delimited-string rdr ch)))

(defn parse-symbol [rdr ch]
  (let [tok (read-token rdr ch)]
    (literal-node tok identity)))

(defn parse-number [rdr ch]
  (let [txt (read-token rdr ch)]
    (number-node txt (edn/read-string txt))))

(defn ?parse-signed-number [rdr ch]
  (case ch
    (\+ \-) (if (digit? (peek-char rdr)) (parse-number rdr ch) rdr)
    ;; \. (if num? :dec-num rdr)
    rdr))

(defn ?parse-number [rdr ch]
  (if (digit? ch)
    (parse-number rdr ch)
    (case ch
      (\+ \-) (if (digit? (peek-char rdr)) (parse-number rdr ch) rdr)
      ;; \. (if num? :dec-num rdr)
      rdr)))

(def parse-number-or-symbol
  (chained-reader-fn ?parse-number parse-symbol))

(defn parse-bool-nil-or-sym [rdr ch]
  (let [txt (read-token rdr ch)]
    (case txt
      "true" (bool-node txt true)
      "false" (bool-node txt false)
      ("nil" "null") (nil-node txt)
      (literal-node txt identity))))

(def parse-number-or-bool-nil-or-sym
  (chained-reader-fn ?parse-number parse-bool-nil-or-sym))

(declare parse)

(defonce ^:private EOF (Object.))

(defn read-delimited-coll
  "Reads and returns a collection ended with delim"
  ^PersistentVector [kind delim rdr]
  (let [[start-line start-column] (#'tr/starting-line-col-info rdr)
        delim (char delim)]
    ;; (char 120)
    ;; (clojure.repl/doc char)
    (loop [a (transient []), ch (peek-char rdr)]
      (if (identical? delim ch) #_(= delim ch)
          (do
            (read-char rdr) ;skip delim
            (persistent! a))
          (let [form (parse rdr false EOF)]
            (if (identical? form EOF)
              (err/throw-eof-delimited rdr kind start-line start-column (count a))
              (recur (conj! a form) (peek-char rdr))))))))

(defn parse-collection [rdr ch]
  (let [open ch
        close (klj.chars/matching-bracket ch)]
    (klj.nodes/sequence-node
     (klj.nodes/delimiter open :open)
     (klj.nodes/delimiter close :close)
     (read-delimited-coll :seq close rdr))))

(defn parse-unmatched-delimiter [rdr ch]
  (err/throw-unmatch-delimiter rdr ch))

;;; Define readers macros that skip over certain token types

(defn skip-char
  "Skip current char, whatever it is.
  Useful for commas, whitespace and EOL characters."
  [rdr _ch] rdr)

(defmacro defskip [tok-name]
  (let [skip-fn-name (symbol (str "skip-" (name tok-name)))
        parse-fn-name (symbol (str "parse-" (name tok-name)))]
    `(do (defn ~skip-fn-name [rdr# ch#]
           (~parse-fn-name rdr# ch#)
           rdr#))))

(defskip space)

(defskip eol)

(defskip string)

(defskip symbol)

(defskip number)

;;; Default reader macros

;; TODO: Make sure map is right data structure here.
;; Prior macro dispatch uses `case` expression which
;; looks at cases in order. Does order matter here?
;; So far, I think not.
(def default-macros
  {\space #'parse-space
   \tab #'parse-space
   \return #'parse-eol
   \newline #'parse-eol
   \" #'parse-string
   \' #'parse-string
   ;; \- parse-num-or-sym
   ;; \+ parse-num-or-sym
   \, #'parse-separator
   \; #'parse-separator
   \: #'parse-separator
   \( #_'parse-delimiter #'parse-collection
   \[ #_parse-delimiter #'parse-collection
   \{ #_parse-delimiter #'parse-collection
   \) #_parse-delimiter #'parse-unmatched-delimiter
   \] #_parse-delimiter #'parse-unmatched-delimiter
   \} #_parse-delimiter #'parse-unmatched-delimiter
   ;;TODO: parse-symbol-or-unicode-whitespace
   :else #'parse-number-or-bool-nil-or-sym #_parse-bool-nil-or-sym #_parse-symbol})

(defn parse-leading [rdr]
  (let [ch (read-char rdr)
        ret (?read-comment rdr ch)]
    (if (identical? rdr ret)
      ch
      ret)))

(def ^:dynamic *macros* default-macros)
(def ^:dynamic *parse-leading* parse-leading #_skip-comments)

(defn parse
  ([reader] (parse reader nil :EOF {}))
  ([reader eof-error? sentinel]
   (parse reader eof-error? sentinel nil {}))
  ([reader eof-error? sentinel opts]
   (parse reader eof-error? sentinel nil opts))
  ([reader eof-error? sentinel return-on _opts]
   (try
     (loop []
       (let [ret (log-source
                  reader
                  ;; *parse-leading* can skip things like
                  ;; comments and whitespace or return a token.
                  ;; If returns char, threat it as first char of next token
                  ;; else return result of *parse-leading*
                  (let [ch (*parse-leading* reader) #_(read-char reader)]
                    (println "parse-lead = " ch)
                    (cond
                      (nil? ch) (if eof-error? (err/throw-eof-error reader nil) sentinel)
                      (= ch return-on) tr/READ_FINISHED
                      (not (char? ch)) ch
                      ;; (rc/number-literal? reader ch) (parse-number reader ch)
                      :else (if-let [f (*macros* ch)]
                              (f reader ch)
                              ((:else *macros*) reader ch)))))]
         (if (identical? ret reader)
           (recur)
           ret)))
     (catch Exception e
       (if (ex-info? e)
         (let [d (ex-data e)]
           (if (= :reader-exception (:type d))
             (throw e)
             (throw (ex-info (.getMessage e)
                             (merge {:type :reader-exception}
                                    d
                                    (when (indexing-reader? reader)
                                      {:line   (get-line-number reader)
                                       :column (get-column-number reader)
                                       :file   (get-file-name reader)}))
                             e))))
         (throw (ex-info (.getMessage e)
                         (merge {:type :reader-exception}
                                (when (indexing-reader? reader)
                                  {:line   (get-line-number reader)
                                   :column (get-column-number reader)
                                   :file   (get-file-name reader)}))
                         e)))))))

(defn parse-all [rdr]
  (let [children
        (loop [ret [], r (parse rdr false EOF)]
          (cond
            (identical? EOF r) ret
            :else (recur (conj ret r) (parse rdr false EOF))))]
    (root-node :klj children)))


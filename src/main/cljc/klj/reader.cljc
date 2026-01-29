(ns klj.reader
  (:refer-clojure :exclude [read read-line read-string char read+string
                            default-data-readers *default-data-reader-fn*
                            *read-eval* *data-readers* *suppress-read*])
  (:require
    [clojure.string :as str]
    #?@(:clj
        [[clojure.java.io :as io]
         [clojure.set :as set]
         [clojure.tools.reader :as tr]
         [clojure.tools.reader.impl.commons :as rc]
         [clojure.tools.reader.impl.errors :as err]
         [clojure.tools.reader.impl.utils :refer [ex-info? #_whitespace?]]
         [clojure.tools.reader.reader-types :refer
          [read-char unread peek-char indexing-push-back-reader indexing-reader? source-logging-push-back-reader source-logging-reader?
           get-line-number get-column-number get-file-name string-push-back-reader log-source]]]
        :cljs
        [[cljs.tools.reader :as tr]
         [cljs.tools.reader.impl.commons :as rc]
         [cljs.tools.reader.impl.errors :as err]
         [cljs.tools.reader.reader-types :as rt]]))
  (:import (clojure.tools.reader.reader_types IndexingPushbackReader SourceLoggingPushbackReader)
           (java.io Writer)
           (java.util List LinkedList)))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (Character/isWhitespace ^Character ch)
        (identical? \,  ch))))

(def kwd-chars-need-quoting #"^[#':]|::|:$|[(){}\[\]\x08 \t\\\r\n\f\v,;\"@^`~]")
(def sym-chars-need-quoting #"^(true|false|nil)$|^[+-]?[0-9]|^[#':]|::|:$|[(){}\[\]\x08 \t\\\r\n\f\v,;\"@^`~]")

(defmethod print-method clojure.lang.Keyword [o, ^Writer w]
  (let [s (.substring (str o) 1)
        quote? (re-find kwd-chars-need-quoting s)]
    (if quote?
      (.write w (str (pr-str s) \:))
      (.write w (str \: s)))))

(defmethod print-method clojure.lang.Symbol [o, ^Writer w]
  ;; Use var since `print-meta` is private
  (#'clojure.core/print-meta o w)
  (let [s (str o)
        quote? (re-find sym-chars-need-quoting s)]
    (if quote?
      (.write w (str (pr-str s) \~))
      (.write w s))))

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

(defn- macro-terminating? [ch]
  (case ch
    (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true
    false))

(defn parse-symbol
  "Parses a string into a vector of the namespace and symbol.
  Doesn't enforce anything"
  [^String token]
  (let [ns-idx (.indexOf token "/")]
    (cond
      (neg? ns-idx) [nil token]
      (pos? ns-idx) [(subs token 0 ns-idx) (subs token (inc ns-idx))]
      (= 1 (count token)) [nil "/"]
      :else [(subs token 0 ns-idx) (subs token (inc ns-idx))])))

(defn read-sym-or [macro-fn]
  (fn [rdr ch opt pending-forms]
    (if (terminating? (peek-char rdr))
      (symbol (str ch))
      (macro-fn rdr ch opt pending-forms))))

;; ## Helpers

(defn read-while
  "Read while the chars fulfill the given condition. Ignores
    the unmatching char."
  ([#?(:cljs ^not-native reader :default reader) p?]
   (read-while reader p? (not (p? nil))))

  ([#?(:cljs ^not-native reader :default reader) p? eof?]
   (let [buf (StringBuffer.)]
     (loop []
       (if-let [c (read-char reader)]
         (if (p? c)
           (do
             (.append buf c)
             (recur))
           (do
             (unread reader c)
             (.toString buf)))
         (if eof?
           (.toString buf)
           (err/throw-eof-error reader nil)))))))

(defn read-until
  "Read until a char fulfills the given condition. Ignores the
   matching char."
  [#?(:cljs ^not-native reader :default reader) p?]
  (read-while
    reader
    (complement p?)
    (p? nil)))

(defn read-to-suffix
  "Reads up to given `suffix`.
  Returns string of chars up to suffix.
  Reader is positioned just _after_ the suffix."
  [#?(:cljs ^not-native reader :default reader) suffix]
  ;; (println "dmk read-to-suffix")
  (let [buf (StringBuffer.)
        n (count suffix)]
    (loop [ix 0 j 1]
      (if (< ix n)
        (let [c (read-char reader)]
          (cond
            (nil? c) (err/throw-eof-error reader nil)
            (= c (nth suffix ix)) (recur (inc ix) (inc j))
            :else (do
                    (.append buf (subs suffix 0 ix))
                    (if (= c (first suffix))
                      (recur 1 (inc j))
                      (do
                        (.append buf c)
                        (recur 0 (inc j)))))))
        (str buf)))))

(defn read-raw-block
  "Helper to read code in a raw block starting with `\\R`
  Reader should be positioned at beginning of optional tag.
  Returns tuple of tag and the content between `lch` and `rch`"
  [#?(:cljs ^not-native reader :default reader) lch rch termch]
  ;; (println "dmk read-raw-block")
  (read-char reader) ;; skip \R
  (let [delim (read-until reader #(= % lch))
        _ (println {:delim delim})
        suffix (str rch delim termch)]
    (read-char reader) ;skip lch
    [delim
     (let [buf (StringBuffer.)
           n (count suffix)]
       (loop [ix 0 j 1]
         (if (< ix n)
           (let [c (read-char reader)]
             (cond
               (nil? c) (err/throw-eof-error reader nil)
               (= c (nth suffix ix)) (recur (inc ix) (inc j))
               :else (do
                       (.append buf (subs suffix 0 ix))
                       (if (= c (first suffix))
                         (recur 1 (inc j))
                         (do
                           (.append buf c)
                           (recur 0 (inc j)))))))
           (.toString buf))))]))

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

(def ^:const upper-limit (int \uD7ff))
(def ^:const lower-limit (int \uE000))

(defn peek-matches? [ch rdr]
  (= (peek-char rdr) ch))

(defn read-char*
  [reader backslash _opts _pending-forms]
  ;; (println "dmk new read-char*" backslash)
  (when (nil? (peek-char reader))
    (err/throw-eof-error reader nil))
  (let [ch (if (#{\\ \" \( \) \{ \} \[ \]}
                 (peek-char reader))
             backslash (read-char reader))
        token (read-token reader ch)
        token-len (count token)
        #_#_[sym-ns sym-name] (parse-symbol token)]
    #_(symbol sym-ns sym-name)
    (cond
      (== 1 token-len) (Character/valueOf (nth token 0))
      (= token "newline") \newline
      (= token "space") \space
      (= token "tab") \tab
      (= token "backspace") \backspace
      (= token "formfeed") \formfeed
      (= token "return") \return
      (.startsWith token "u")
      (let [c (#'tr/read-unicode-char token 1 4 16)
            ic (int c)]
        (if (and (> ic upper-limit)
                 (< ic lower-limit))
          (err/throw-invalid-character-literal reader (Integer/toString ic 16))
          c))
      (.startsWith token "o")
      (let [len (dec token-len)]
        (if (> len 3)
          (err/throw-invalid-octal-len reader token)
          (let [uc (#'tr/read-unicode-char token 1 len 8)]
            (if (> (int uc) 0377)
              (err/throw-bad-octal-number reader)
              uc))))
      :else (let [[sym-ns sym-name] (parse-symbol token)]
              (if (peek-matches? \: reader)
                (do (read-char reader)
                    (keyword sym-ns sym-name))
                (symbol sym-ns sym-name))))))

#_(defn read-char*
  "Read in a character literal"
  [rdr _backslash _opts _pending-forms]
  (println "dmk read-char*")
  (let [ch (read-char rdr)]
    (if-not (nil? ch)
      (let [token (if (or (#'tr/macro-terminating? ch)
                          (whitespace? ch))
                    (str ch)
                    (read-token rdr ch))
            token-len (count token)]
        (cond
          (== 1 token-len)  (Character/valueOf (nth token 0))

          (= token "newline") \newline
          (= token "space") \space
          (= token "tab") \tab
          (= token "backspace") \backspace
          (= token "formfeed") \formfeed
          (= token "return") \return

          (.startsWith token "u")
          (let [c (#'tr/read-unicode-char token 1 4 16)
                ic (int c)]
            (if (and (> ic #'tr/upper-limit)
                     (< ic #'tr/lower-limit))
              (err/throw-invalid-character-literal rdr (Integer/toString ic 16))
              c))

          (.startsWith token "o")
          (let [len (dec token-len)]
            (if (> len 3)
              (err/throw-invalid-octal-len rdr token)
              (let [uc (#'tr/read-unicode-char token 1 len 8)]
                (if (> (int uc) 0377)
                  (err/throw-bad-octal-number rdr)
                  uc))))

          :else (err/throw-unsupported-character rdr token)))
      (err/throw-eof-in-character rdr))))

(defn read-comment
  [reader _initch _opts _pending-forms]
  ;; (println "dmk read-comment")
  (rc/read-comment reader _initch _opts _pending-forms))

(defn read-comment
  [reader _initch _opts _pending-forms]
  ;; (println "dmk raw read-comment")
  (if (not= \\ (peek-char reader))
    (rc/skip-line reader)
    (do
      (read-char reader) ;; skip \\
      (if (= \R (peek-char reader))
        (let [[_delim _s] (read-raw-block reader \( \) \;)]
          reader)
        (rc/skip-line reader)))))

(defn read-number
  [reader _initch]
  ;; (println "dmk read-number")
  (#'tr/read-number reader _initch))

(defn string-or-char [q s]
  (case q
    \' (if (= (count s) 1) (first s) s)
    \" s))

(defn- read-delimited-string
  [reader quote-ch _opts _pending-forms]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (cond
      (nil? ch) (err/throw-eof-reading reader :string sb)
      (= \\ ch) (recur (doto sb (.append (#'tr/escape-char reader)))
                       (read-char reader))
      (= quote-ch ch) (if (= (peek-char reader) quote-ch)
                        (do
                          (read-char reader) ;skip quote-ch
                          (recur (doto sb (.append ch)) (read-char reader)))
                        (str sb))
      :else (recur (doto sb (.append ch)) (read-char reader)))))

(defn read-quoted-name*
  [reader initch _opts _pending-forms]
  (if (not= \\ (peek-char reader))
    (read-delimited-string reader initch _opts _pending-forms)
    (do
      (read-char reader) ;; skip \\
      (if (= \R (peek-char reader))
        (let [[_delim s] (read-raw-block reader \( \) initch)]
          s)
        (do
          (unread reader \\)
          (read-delimited-string reader initch _opts _pending-forms))))))

(defn read-quoted-name
  [reader initch _opts _pending-forms]
  ;; (println "dmk read-quoted-name")
  (let [s (read-quoted-name* reader initch _opts _pending-forms)
        ch (read-char reader)]
    (case ch
      (\: \k) (keyword s)
      (\~ \s) (symbol s)
      \c (if (= 1 (count s))
           (first s)
           (vec s))
      (do (unread reader ch) (string-or-char initch s)))))

(defn- read-symbol
  [rdr initch]
  ;; (println "dmk new read-symbol")
  (let [[line column] (#'tr/starting-line-col-info rdr)]
    (when-let [token (read-token rdr initch)]
      (case token

        ;; special symbols
        "nil" nil
        "true" true
        "false" false
        "/" '/

        (or (when-let [[sym-ns sym-name :as p] (parse-symbol token)]
              ;; peek for \:
              (if (peek-matches? \: rdr)
                (do (read-char rdr) ;skip \:
                    (keyword sym-ns sym-name))
                (with-meta
                  (symbol (p 0) (p 1))
                  (when line
                    (merge
                      (when-let [file (get-file-name rdr)]
                        {:file file})
                      (let [[end-line end-column] (#'tr/ending-line-col-info rdr)]
                        {:line line
                         :column column
                         :end-line end-line
                         :end-column end-column}))))))
            (err/throw-invalid rdr :symbol token))))))

(declare read-klj)

(defn read-arg
  [rdr pct opts pending-forms]
  (if-not (thread-bound? #'tr/arg-env)
    (read-symbol rdr pct)
    (let [ch (peek-char rdr)]
      (cond
       (or (whitespace? ch)
           (macro-terminating? ch)
           (nil? ch))
       (#'tr/register-arg 1)

       (identical? ch \&)
       (do (read-char rdr)
           (#'tr/register-arg -1))

       :else
       (let [n (read-klj rdr true nil opts pending-forms)]
         (if-not (integer? n)
           (throw (IllegalStateException. "Arg literal must be %, %& or %integer"))
           (#'tr/register-arg n)))))))

(defn read-escaped-symbol
  [reader _initch _opts _pending-forms]
  ;; (println "dmk read-escaped-symbol" _initch)
  (when (nil? (peek-char reader))
    (err/throw-eof-error reader nil))
  (let [ch (if (#{\b \f \n \o \r \s \t \u
                  \\ \" \( \) \{ \} \[ \]}
                 (peek-char reader))
             _initch (read-char reader))
        token (read-token reader ch)
        [sym-ns sym-name] (parse-symbol token)]
    (if (peek-matches? \: reader)
      (do (read-char reader)
          (keyword sym-ns sym-name))
      (symbol sym-ns sym-name))))

(defn read-keyword
  [reader _initch _opts _pending-forms]
  ;; (println "dmk read-keyword")
  #_(#'tr/read-keyword reader _initch _opts _pending-forms)
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader ch)
            ;; _ (println "dmk token " token)
            s (parse-symbol token)]
        (if s
          (let [^String ns (s 0)
                ^String name (s 1)]
            (if (identical? \: (nth token 0))
              (if ns
                (let [ns (#'tr/resolve-alias (symbol (subs ns 1)))]
                  (if ns
                    (keyword (str ns) name)
                    (err/throw-invalid reader :keyword (str \: token))))
                (keyword (str *ns*) (subs name 1)))
              (keyword ns name)))
          (err/throw-invalid reader :keyword (str \: token))))
      (err/throw-single-colon reader))))

(defn- read-symbolic-value
  [rdr _ opts pending-forms]
  (let [sym (read-klj rdr true nil opts pending-forms)]
    (case sym
      Inf Double/POSITIVE_INFINITY
      +Inf Double/POSITIVE_INFINITY
      -Inf Double/NEGATIVE_INFINITY
      NaN Double/NaN
      PI Math/PI
      E Math/E
      (err/reader-error rdr (str "Invalid token: ##" sym)))))

(declare read-dispatch)

(def read-sym-or-comment (read-sym-or read-comment))
(def read-sym-or-deref
  (read-sym-or (#'tr/wrapping-reader 'clojure.core/deref)))
(def read-sym-or-keyword (read-sym-or read-keyword))
(def read-sym-or-meta (read-sym-or #'tr/read-meta))
(def read-sym-or-quote
  (read-sym-or (#'tr/wrapping-reader 'quote)))
(def read-sym-or-syntax-quote (read-sym-or #'tr/read-syntax-quote))
(def read-sym-or-unquote (read-sym-or #'tr/read-unquote))
(def read-sym-or-char (read-sym-or read-char*))
(def read-sym-or-dispatch (read-sym-or #'read-dispatch))

(defn macros [ch]
  (case ch
    \" read-quoted-name
    ;; \' read-quoted-name
    \: read-sym-or-keyword #_read-keyword
    \; read-sym-or-comment
    #_\‘ \' read-sym-or-quote #_(#'tr/wrapping-reader 'quote)
    \@ read-sym-or-deref #_(#'tr/wrapping-reader 'clojure.core/deref)
    \^ read-sym-or-meta #_#'tr/read-meta
    \` read-sym-or-syntax-quote #_#'tr/read-syntax-quote
    \~ read-sym-or-unquote #_#'tr/read-unquote
    \( #'tr/read-list
    \) #'tr/read-unmatched-delimiter
    \[ #'tr/read-vector
    \] #'tr/read-unmatched-delimiter
    \{ #'tr/read-map
    \} #'tr/read-unmatched-delimiter
    \\ read-sym-or-char #_read-char*
    \% read-arg
    \# read-sym-or-dispatch #_read-dispatch
    nil))

(defn dispatch-macros [ch]
  (case ch
    \^ #'tr/read-meta                ;deprecated
    \' (#'tr/wrapping-reader 'var)
    \( #'tr/read-fn
    \= #'tr/read-eval
    \{ #'tr/read-set
    \< (#'rc/throwing-reader "Unreadable form")
    \" #'tr/read-regex
    \! #'rc/read-comment
    \_ #'tr/read-discard
    \? #'tr/read-cond
    \: #'tr/read-namespaced-map
    \# read-symbolic-value
    \\ read-escaped-symbol
    nil))

(defn read-dispatch
  [rdr _ch opts pending-forms]
  (if-let [ch (read-char rdr)]
    (case ch
      ;; (\t \T) true  ; TODO: ensure boundary after
      ;; (\f \F) false ; TODO: ensure boundary after
      (if-let [dm (dispatch-macros ch)]
        (dm rdr ch opts pending-forms)
        ;; TODO: write read-bool-or-tagged
        ;; to read #t as true and #f as false
        (#'tr/read-tagged (doto rdr (unread ch)) ch opts pending-forms))) ;; ctor reader is implemented as a tagged literal
    (err/throw-eof-at-dispatch rdr)))

;;; DMK: Copied from clojure.tools.reader ns
;;; so that `read*` uses my `macros`, `read-number` and
;;; `read-symbol`.

(defn skip-comments [reader]
  (when-let [ch (read-char reader)]
    (case ch
      \# (case (peek-char reader)
           \space (do (rc/skip-line reader) (recur reader))
           \| (do (read-to-suffix reader "|#") (recur reader))
           ch)
      \/ (case (peek-char reader)
           \/ (do (rc/skip-line reader) (recur reader))
           \* (do (read-to-suffix reader "*/") (recur reader))
           ch)
      \; (let [ch2 (peek-char reader)
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
             (do (read-to-suffix reader suffix) (recur reader))
             ch))
      ch)))

(defn read-klj
  ([reader eof-error? sentinel opts pending-forms]
     (read-klj reader eof-error? sentinel nil opts pending-forms))
  ([reader eof-error? sentinel return-on opts pending-forms]
     (when (= :unknown tr/*read-eval*)
       (err/reader-error "Reading disallowed - *read-eval* bound to :unknown"))
     (try
       (loop []
         (let [ret (log-source reader
                     (if (seq pending-forms)
                       (.remove ^List pending-forms 0)
                       (let [ch (skip-comments reader)]
                         (cond
                           (whitespace? ch) reader
                           (nil? ch) (if eof-error? (err/throw-eof-error reader nil) sentinel)
                           (= ch return-on) tr/READ_FINISHED
                           (rc/number-literal? reader ch) (read-number reader ch)
                           :else (if-let [f (macros ch)]
                                   (f reader ch opts pending-forms)
                                   (read-symbol reader ch))))))]
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

(defn read [& args]
  (binding [tr/read* read-klj]
     (apply tr/read args)))

(defn read-string [& args]
  (binding [tr/read* read-klj]
     (apply tr/read-string args)))

(defn read+string [& args]
  (binding [tr/read* read-klj]
     (apply tr/read-string args)))

#?(:clj
   (defn file-reader
     "Create reader for files."
     ^IndexingPushbackReader
     [f]
     (-> (io/file f)
         (io/reader)
         (indexing-push-back-reader 2))))

(defn string-reader
  "Create reader for strings."
  [s]
  (indexing-push-back-reader s 2))

(defn load-reader [rdr]
  (let [EOF (Object.)]
     (loop [ret nil, r (read rdr false EOF)]
       (cond
         (identical? EOF r) ret
         :else (recur (eval r) (read rdr false EOF))))))

(defn load-string [s]
  (load-reader (string-reader s)))

(defn load-file [f]
  (load-reader (file-reader f)))


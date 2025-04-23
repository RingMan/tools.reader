(ns klj.reader
  (:refer-clojure :exclude [read read-line read-string char read+string
                            default-data-readers *default-data-reader-fn*
                            *read-eval* *data-readers* *suppress-read*])
  (:require
    [clojure.string :as str]
    #?@(:clj
        [[clojure.set :as set]
         [clojure.tools.reader :as tr]
         [clojure.tools.reader.impl.commons :as rc]
         [clojure.tools.reader.impl.errors :as err]
         [clojure.tools.reader.impl.utils :refer [ex-info? whitespace?]]
         [clojure.tools.reader.reader-types :refer
          [read-char unread peek-char indexing-reader? source-logging-push-back-reader source-logging-reader?
           get-line-number get-column-number get-file-name string-push-back-reader log-source]]]
        :cljs
        [[cljs.tools.reader :as tr]
         [cljs.tools.reader.impl.commons :as rc]
         [cljs.tools.reader.impl.errors :as err]
         [cljs.tools.reader.reader-types :as rt]]))
  (:import (clojure.tools.reader.reader_types SourceLoggingPushbackReader)
           (java.util List LinkedList)))

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

(defn read-raw-block
  "Helper to read code in a raw block starting with `\\R`
  Reader should be positioned at beginning of optional tag.
  Returns tuple of tag and the content between `lch` and `rch`"
  [#?(:cljs ^not-native reader :default reader) lch rch termch]
  (println "dmk read-raw-block")
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
  (loop [sb (StringBuilder.) ch initch]
    (cond
      (nil? ch) (str sb)
      (token-terminating? ch) (do (unread reader ch)
                                  (str sb))
      (identical? \\ ch) (recur (doto sb (.append (#'tr/escape-char reader)))
                                (read-char reader))
      :else (recur (doto sb (.append ch)) (read-char reader)))))

#_(defn read-char*
  [reader _initch _opts _pending-forms]
  (println "dmk read-char*")
  (#'tr/read-char* reader _initch _opts _pending-forms))

(defn- read-char*
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
  (println "dmk read-comment")
  (rc/read-comment reader _initch _opts _pending-forms))

(defn read-comment
  [reader _initch _opts _pending-forms]
  (println "dmk raw read-comment")
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
  (println "dmk read-number")
  (#'tr/read-number reader _initch))

(defn read-string*
  [reader _initch _opts _pending-forms]
  (println "dmk read-string*")
  (if (not= \\ (peek-char reader))
    (#'tr/read-string* reader _initch _opts _pending-forms)
    (do
      (read-char reader) ;; skip \\
      (if (= \R (peek-char reader))
        (let [[_delim s] (read-raw-block reader \( \) \")]
          s)
        (do
          (unread reader \\)
          (#'tr/read-string* reader _initch _opts _pending-forms))))))

(defn read-quoted-name
  [reader _initch _opts _pending-forms]
  (println "dmk read-quoted-name")
  (let [s (read-string* reader _initch _opts _pending-forms)
        ch (read-char reader)]
    (case ch
      (\: \k) (keyword s)
      (\' \~ \s) (symbol s)
      \c (first s) ; TODO: ensure length is one
      (do (unread reader ch) s))))

#_(defn read-symbol
  [reader _initch]
  (println "dmk read-symbol")
  (#'tr/read-symbol reader _initch))

(defn- read-symbol
  [rdr initch]
  (println "dmk new read-symbol")
  (let [[line column] (#'tr/starting-line-col-info rdr)]
    (when-let [token (read-token rdr initch)]
      (case token

        ;; special symbols
        "nil" nil
        "true" true
        "false" false
        "/" '/

        (or (when-let [p (parse-symbol token)]
              (with-meta (symbol (p 0) (p 1))
                (when line
                  (merge
                   (when-let [file (get-file-name rdr)]
                     {:file file})
                   (let [[end-line end-column] (#'tr/ending-line-col-info rdr)]
                     {:line line
                      :column column
                      :end-line end-line
                      :end-column end-column})))))
            (err/throw-invalid rdr :symbol token))))))

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
       (let [n (read* rdr true nil opts pending-forms)]
         (if-not (integer? n)
           (throw (IllegalStateException. "Arg literal must be %, %& or %integer"))
           (#'tr/register-arg n)))))))

(defn read-escaped-symbol
  [reader _initch _opts _pending-forms]
  (println "dmk read-escaped-symbol")
  (#'tr/read-symbol reader _initch))

(defn read-keyword
  [reader _initch _opts _pending-forms]
  (println "dmk read-keyword")
  #_(#'tr/read-keyword reader _initch _opts _pending-forms)
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader ch)
            _ (println "dmk token " token)
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
(def read-sym-or-dispatch (read-sym-or read-dispatch))

(defn macros [ch]
  (case ch
    \" read-quoted-name
    \: read-sym-or-keyword #_read-keyword
    \; read-sym-or-comment
    \' read-sym-or-quote #_(#'tr/wrapping-reader 'quote)
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
    \% #'tr/read-arg
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
    \# #'tr/read-symbolic-value
    \\ read-escaped-symbol
    nil))

(defn read-dispatch
  [rdr _ch opts pending-forms]
  (if-let [ch (read-char rdr)]
    (case ch
      (\t \T) true  ; TODO: ensure boundary after
      (\f \F) false ; TODO: ensure boundary after
      (if-let [dm (dispatch-macros ch)]
        (dm rdr ch opts pending-forms)
        ;; TODO: write read-bool-or-tagged
        ;; to read #t as true and #f as false
        (#'tr/read-tagged (doto rdr (unread ch)) ch opts pending-forms))) ;; ctor reader is implemented as a tagged literal
    (err/throw-eof-at-dispatch rdr)))

;;; DMK: Copied from clojure.tools.reader ns
;;; so that `read*` uses my `macros`, `read-number` and
;;; `read-symbol`.

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
                       (let [ch (read-char reader)]
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


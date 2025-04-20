(ns klj.reader
  (:refer-clojure :exclude [read read-line read-string char read+string
                            default-data-readers *default-data-reader-fn*
                            *read-eval* *data-readers* *suppress-read*])
  (:require
    [clojure.string :as str]
    #?@(:clj
        [[clojure.tools.reader :as tr]
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

(defn read-char*
  [reader _initch _opts _pending-forms]
  (println "dmk read-char*")
  (#'tr/read-char* reader _initch _opts _pending-forms))

(defn read-comment
  [reader _initch _opts _pending-forms]
  (println "dmk read-comment")
  (rc/read-comment reader _initch _opts _pending-forms))

(defn read-number
  [reader _initch]
  (println "dmk read-number")
  (#'tr/read-number reader _initch))

(defn read-string*
  [reader _initch _opts _pending-forms]
  (println "dmk read-string*")
  (#'tr/read-string* reader _initch _opts _pending-forms))

(defn read-symbol
  [reader _initch]
  (println "dmk read-symbol")
  (#'tr/read-symbol reader _initch))

(defn read-keyword
  [reader _initch _opts _pending-forms]
  (println "dmk read-keyword")
  (#'tr/read-keyword reader _initch _opts _pending-forms)
  #_(err/throw-invalid reader :keyword "dmk just throw for now")
  #_(let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader :keyword ch)
            s (parse-symbol token)]
        (if s
          (let [^String ns (s 0)
                ^String name (s 1)]
            (if (identical? \: (nth token 0))
              (if ns
                (let [ns (resolve-alias (symbol (subs ns 1)))]
                  (if ns
                    (keyword (str ns) name)
                    (err/throw-invalid reader :keyword (str \: token))))
                (keyword (str *ns*) (subs name 1)))
              (keyword ns name)))
          (err/throw-invalid reader :keyword (str \: token))))
      (err/throw-single-colon reader))))

(declare read-dispatch)

(defn macros [ch]
  (case ch
    \" read-string*
    \: read-keyword #_#'tr/read-keyword
    \; read-comment
    \' (#'tr/wrapping-reader 'quote)
    \@ (#'tr/wrapping-reader 'clojure.core/deref)
    \^ #'tr/read-meta
    \` #'tr/read-syntax-quote ;;(wrapping-reader 'syntax-quote)
    \~ #'tr/read-unquote
    \( #'tr/read-list
    \) #'tr/read-unmatched-delimiter
    \[ #'tr/read-vector
    \] #'tr/read-unmatched-delimiter
    \{ #'tr/read-map
    \} #'tr/read-unmatched-delimiter
    \\ read-char*
    \% #'tr/read-arg
    \# read-dispatch #_#'tr/read-dispatch
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
    nil))

(defn read-dispatch
  [rdr _ch opts pending-forms]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch opts pending-forms)
      ;; TODO: write read-bool-or-tagged
      ;; to read #t as true and #f as false
      (#'tr/read-tagged (doto rdr (unread ch)) ch opts pending-forms)) ;; ctor reader is implemented as a tagged literal
    (err/throw-eof-at-dispatch rdr)))

(comment
  (tr/read-string ":kwd")
  (alter-var-root #'tr/macros (constantly macros))
  (alter-var-root #'tr/dispatch-macros (constantly dispatch-macros)))

;;; DMK: Copied from clojure.tools.reader ns
;;; so that `read*` uses my `macros` instead of the default

(defn read*
  ([reader eof-error? sentinel opts pending-forms]
     (read* reader eof-error? sentinel nil opts pending-forms))
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
                           (= ch return-on) #'tr/READ_FINISHED
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

(defn read
  "Reads the first object from an IPushbackReader or a java.io.PushbackReader.
   Returns the object read. If EOF, throws if eof-error? is true.
   Otherwise returns sentinel. If no stream is provided, *in* will be used.

   Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw

   ***WARNING***
   Note that read can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read

   Note that the function signature of clojure.tools.reader/read and
   clojure.tools.reader.edn/read is not the same for eof-handling"
  {:arglists '([] [reader] [opts reader] [reader eof-error? eof-value])}
  ([] (read *in* true nil))
  ([reader] (read reader true nil))
  ([{eof :eof :as opts :or {eof :eofthrow}} reader]
   (when (source-logging-reader? reader)
     (let [^StringBuilder buf (:buffer @(.source-log-frames ^SourceLoggingPushbackReader reader))]
       (.setLength buf 0)))
   (read* reader (= eof :eofthrow) eof nil opts (LinkedList.)))
  ([reader eof-error? sentinel]
   (when (source-logging-reader? reader)
     (let [^StringBuilder buf (:buffer @(.source-log-frames ^SourceLoggingPushbackReader reader))]
       (.setLength buf 0)))
   (read* reader eof-error? sentinel nil {} (LinkedList.))))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   ***WARNING***
   Note that read-string can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read-string

   Note that the function signature of clojure.tools.reader/read-string and
   clojure.tools.reader.edn/read-string is not the same for eof-handling"
  ([s]
     (read-string {} s))
  ([opts s]
     (when (and s (not (identical? s "")))
       (read opts (string-push-back-reader s)))))

(defn read+string
  "Like read, and taking the same args. reader must be a SourceLoggingPushbackReader.
  Returns a vector containing the object read and the (whitespace-trimmed) string read."
  ([] (read+string (source-logging-push-back-reader *in*)))
  ([stream] (read+string stream true nil))
  ([^SourceLoggingPushbackReader stream eof-error? eof-value]
   (let [^StringBuilder buf (doto ^StringBuilder (:buffer @(.source-log-frames stream)) (.setLength 0))
         o (log-source stream (read stream eof-error? eof-value))
         s (.trim (str buf))]
     [o s]))
  ([opts ^SourceLoggingPushbackReader stream]
   (let [^StringBuilder buf (doto ^StringBuilder (:buffer @(.source-log-frames stream)) (.setLength 0))
         o (log-source stream (read opts stream))
         s (.trim (str buf))]
     [o s])))

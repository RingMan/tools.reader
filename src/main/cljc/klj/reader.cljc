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

(defn read-escaped-symbol
  [reader _initch _opts _pending-forms]
  (println "dmk read-escaped-symbol")
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
  (binding [tr/read* read*]
     (apply tr/read args)))

(defn read-string [& args]
  (binding [tr/read* read*]
     (apply tr/read-string args)))

(defn read+string [& args]
  (binding [tr/read* read*]
     (apply tr/read-string args)))


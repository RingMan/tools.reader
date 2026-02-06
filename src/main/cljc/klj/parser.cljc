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
            [klj.blocks :refer [chained-reader-fn read-delimited-string read-n read-to-eol ?read-comment] :as kb]
            [klj.chars :refer [digit? eol-ch? whitespace?]]
            [klj.nodes :refer [as-node bool-node eol-node line-comment-node string-node token-node nil-node number-node punctuator code-node whitespace] :as kn]
            [klj.reader :as k]
            [clojure.core :as c])
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
      (identical? \\ ch) (let [ch2 (read-char reader)]
                           (.append sb ch)
                           (if (nil? ch2)
                             (err/throw-eof-reading reader :token sb)
                             (recur (.append sb ch2) (read-char reader) false)))
      (or first? (not (#{\: \; \,} ch)))
        (recur (.append sb ch) (read-char reader) false)
      (terminating? (peek-char reader)) (do (unread reader ch) (str sb))
      (opening-delim? (peek-char reader))
        (throw (IllegalStateException. "Expected EOF, white space or closing delimiter after punctuation at end of token."))
      :else (recur (.append sb ch) (read-char reader) false))))

(defn parse-space [rdr ch]
  (let [buf (StringBuffer.)]
    (loop [c ch]
      (if (whitespace? c)
        (do (.append buf c) (recur (read-char rdr)))
        (do (unread rdr c) (whitespace (str buf)))))))

(defn ?parse-space [rdr ch]
  (if (whitespace? ch)
    (parse-space rdr ch)
    rdr))

(defn parse-separator [_rdr ch]
  (punctuator ch))

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

(defn parse-comment
  ([rdr ch]
   (parse-comment rdr ch nil))
  ([_rdr ch ch2]
   (let [[body eol] (read-to-eol _rdr)
         eol (or eol "")]
     (line-comment-node (str ch ch2) eol body))))

(defn parse-backslash
  [rdr backslash]
  (when (nil? (peek-char rdr))
    (err/throw-eof-error rdr nil))
  (let [[ch ch2] (if (#{\\ \" \( \) \{ \} \[ \]}
                       (peek-char rdr))
                   [nil backslash] [backslash (read-char rdr)])
        token (read-token rdr ch2)
        text (str ch token)
        text-len (count text)]
    (cond
      (== 2 text-len) (kn/character-node text (k/read-string text))
      (contains? #{"newline" "space" "tab" "backspace" "formfeed" "return"} token)
        (kn/character-node text (k/read-string text))
      (.startsWith token "u") (kn/character-node text (k/read-string text))
      (.startsWith token "o") (kn/character-node text (k/read-string text))
      :else (let [;; add comma to ensure trailing \, \; or \: in token are read
                  sym (k/read-string (str text \,))
                  sym-ns (namespace sym)
                  sym-name (name sym)]
              (if (k/peek-matches? \: rdr)
                (kn/keyword-node (str text (read-char rdr)) sym-ns sym-name)
                (kn/symbol-node text sym-ns sym-name))))))

(defn parse-sym-or-backslash [rdr ch]
  (if (k/nil-or-ws? (peek-char rdr))
    (as-node (symbol (str ch)))
    (parse-backslash rdr ch)))

(defn parse-string [rdr ch]
  (string-node ch ch (read-delimited-string rdr ch)))

(defn parse-quoted-name
  [reader initch]
  (let [s (kb/read-quoted-name reader initch)
        ch (read-char reader)]
    (case ch
      (\: \k \~ \s \c) (kn/string-node initch initch ch s)
      (do (unread reader ch) (kn/string-node initch initch s)))))

(defn parse-keyword [rdr ch]
  (let [tok (read-token rdr ch)
        ;; add comma to ensure trailing \, \; or \: in token are read
        kwd (k/read-string (str tok \,))
        k-ns (namespace kwd)
        k-name (name kwd)]
    (kn/keyword-node tok k-ns k-name)))

(defn parse-symbol [rdr ch]
  (let [tok (read-token rdr ch)
        ;; add comma to ensure trailing \, \; or \: in token are read
        sym (k/read-string (str tok \,))]
    (cond
      (nil? sym) (kn/nil-node tok)
      (boolean? sym) (kn/bool-node tok sym)
      :else (let [s-ns (namespace sym)
                  s-name (name sym)]
              (kn/symbol-node tok s-ns s-name)))))

(defn parse-escaped-symbol
  [rdr ch ch2]
  ;; (println "dmk read-escaped-symbol" ch)
  (when (nil? (peek-char rdr))
    (err/throw-eof-error rdr nil))
  (let [[ch2 ch3] (if (#{\b \f \n \o \r \s \t \u
                         \\ \" \( \) \{ \} \[ \]}
                        (peek-char rdr))
                    [nil ch2]
                    [ch2 (read-char rdr)])
        tok (read-token rdr ch3)
        text (str ch ch2 tok)
        ;; add comma to ensure trailing \, \; or \: in token are read
        sym (k/read-string (str text \,))
        s-ns (namespace sym)
        s-name (name sym)]
    (if (k/peek-matches? \: rdr)
      (kn/keyword-node (str text (read-char rdr)) s-ns s-name)
      (kn/symbol-node text s-ns s-name))))

(defn parse-symbolic [rdr ch ch2]
  (let [tok (read-token rdr ch)]
    (kn/symbolic-node (str ch ch2) (subs tok 1))))

(def parse-whitespace-or-symbol
  (chained-reader-fn ?parse-space parse-symbol))

(defn parse-number [rdr ch]
  (let [txt (read-token rdr ch)
        n (edn/read-string txt)
        txt' (pr-str n)]
    (if (= txt txt')
      (number-node txt n)
      (throw (ex-info (str "Invalid numeric literal: " txt) {})))))

(defn ?parse-signed-number [rdr ch]
  (case ch
    (\+ \-) (if (digit? (peek-char rdr)) (parse-number rdr ch) rdr)
    rdr))

(defn ?parse-number [rdr ch]
  (if (digit? ch)
    (parse-number rdr ch)
    (case ch
      (\+ \-) (if (digit? (peek-char rdr)) (parse-number rdr ch) rdr)
      rdr)))

(def parse-number-or-symbol
  (chained-reader-fn ?parse-number parse-symbol))

(def parse-whitespace-number-or-symbol
  (chained-reader-fn ?parse-space parse-number-or-symbol))

(defn parse-bool-nil-or-sym [rdr ch]
  (let [txt (read-token rdr ch)]
    (case txt
      "true" (bool-node txt true)
      "false" (bool-node txt false)
      ("nil" "null") (nil-node txt)
      (token-node txt txt))))

(def parse-number-or-bool-nil-or-sym
  (chained-reader-fn ?parse-number parse-bool-nil-or-sym))

(declare parse)

(defonce ^:private EOF (Object.))

(defn read-delimited-coll
  "Reads and returns a collection ended with delim"
  ^PersistentVector [kind delim rdr]
  (let [[start-line start-column] (#'tr/starting-line-col-info rdr)
        delim (char delim)]
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

(defn parse-set [rdr _ch ch2]
  (let [close (klj.chars/matching-bracket ch2)]
    (klj.nodes/set-node
     (read-delimited-coll :set close rdr))))

(declare parse-sexprs)

(defn parse-deref [rdr _ch]
  (kn/deref-node (parse-sexprs rdr :deref 1)))

(defn parse-eval [rdr _ch _ch2]
  (kn/eval-node (parse-sexprs rdr :eval 1)))

(defn parse-fn [rdr _ch ch2]
  (unread rdr ch2)
  (kn/fn-node (parse-sexprs rdr :fn 1)))

(defn parse-meta
  ([rdr ch]
   (parse-meta rdr ch nil))
  ([rdr ch ch2]
   (kn/meta-node (str ch ch2) (parse-sexprs rdr :meta 2))))

(defn parse-quote [rdr _ch]
  (kn/quote-node (parse-sexprs rdr :quote 1)))

(defn parse-syntax-quote [rdr _ch]
  (kn/syntax-quote-node (parse-sexprs rdr :syntax-quote 1)))

(defn parse-unquote [rdr _ch]
  (let [ch (peek-char rdr)]
    (if (= ch \@)
      (kn/unquote-splicing-node (parse-sexprs rdr :unquote-splicing 1 true))
      (kn/unquote-node (parse-sexprs rdr :unquote 1)))))

(defn parse-var [rdr _ch _ch2]
  (kn/var-node (parse-sexprs rdr :syntax-quote 1)))

(defn parse-regex [rdr ch ch2]
  (kn/regex-node (str ch ch2) ch2 (read-delimited-string rdr ch2)))

(defn parse-discard [rdr _ch _ch2]
  (kn/discard-node (parse-sexprs rdr :uneval 1)))

(defn parse-rdr-cond [rdr ch ch2]
  (let [ch3 (peek-char rdr)]
    (if (= ch3 \@)
      (kn/conditional-splicing-node (parse-sexprs rdr :conditional-splicing 1 true))
      (kn/conditional-node (parse-sexprs rdr :conditional 1)))))

(defn parse-ns-map [rdr ch ch2]
  (let [ch3 (peek-char rdr)]
    (if (= ch3 \:)
      (do (read-char rdr) ;; skip \:
          (kn/ns-map-node (cons (kn/auto-resolve-node)
                                (parse-sexprs rdr :map 1))))
      (let [tok (read-token rdr ch2)
            _ (println {:tok tok})
            [k-ns k-name] (k/parse-symbol (subs tok 1))]
        (kn/ns-map-node (cons (kn/keyword-node tok k-ns k-name)
                              (parse-sexprs rdr :map 1)))))))

(defn parse-tag [rdr ch]
  (kn/tag-node (parse-sexprs rdr :tag 2)))

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

(defn parse-sym-or [macro-fn]
  (fn [rdr ch]
    (if (terminating? (peek-char rdr))
      (as-node (symbol (str ch)))
      (macro-fn rdr ch))))

(def parse-sym-or-comment (parse-sym-or parse-comment))
(def parse-sym-or-deref
  (parse-sym-or parse-deref))
(def parse-sym-or-keyword (parse-sym-or parse-keyword))
(def parse-sym-or-meta (parse-sym-or parse-meta))
(def parse-sym-or-quote
  (parse-sym-or parse-quote))
(def parse-sym-or-syntax-quote (parse-sym-or parse-syntax-quote))
(def parse-sym-or-unquote (parse-sym-or parse-unquote))
;; (def parse-sym-or-char (parse-sym-or read-char*))
(declare parse-dispatch)
(def parse-sym-or-dispatch (parse-sym-or parse-dispatch))

;;; Default reader macros

(def dispatch-ch \#)

(defn- parse-sexprs
  [#?(:cljs ^not-native reader :default reader) node-tag n & [ignore?]]
  (when ignore?
    (read-char reader))
  (read-n
   reader
   node-tag
   parse
   #(case (::kn/type %)
      (:comment :line-comment :delimiter :eol :space :punctuator) false
      true)
   n))

(def default-dispatch-macros
  {\! #'parse-comment
   \^ #'parse-meta
   \' #'parse-var
   \= #'parse-eval
   \" #'parse-regex
   \{ #'parse-set
   \# #'parse-symbolic
   \( #'parse-fn
   \_ #'parse-discard
   \? #'parse-rdr-cond
   \: #'parse-ns-map
   \\ #'parse-escaped-symbol
   :else #'parse-tag})

(defn parse-dispatch [rdr ch]
  (let [ch2 (read-char rdr)
        f (default-dispatch-macros ch2)]
    (cond
      (nil? ch2) (throw (ex-info "Expected dispatch macro char or tag, got EOF" {}))
      (nil? f) (do (unread rdr ch2)
                   ((:else default-dispatch-macros) rdr ch))
      #_(throw (ex-info "No such dispatch macro" {:ch ch :ch2 ch2}))
      :else (f rdr ch ch2))))

;; TODO: Make sure map is right data structure here.
;; Prior macro dispatch uses `case` expression which
;; looks at cases in order. Does order matter here?
;; So far, I think not.
(def default-macros
  {\space #'parse-space
   \tab #'parse-space
   \return #'parse-eol
   \newline #'parse-eol
   \\ #'parse-sym-or-backslash
   \" #'parse-quoted-name
   ;; \' #'parse-string
   ;; \- parse-num-or-sym
   ;; \+ parse-num-or-sym
   \, #'parse-separator
   \; #'parse-sym-or-comment
   \: #'parse-sym-or-keyword
   \( #_'parse-delimiter #'parse-collection
   \[ #_parse-delimiter #'parse-collection
   \{ #_parse-delimiter #'parse-collection
   \) #_parse-delimiter #'parse-unmatched-delimiter
   \] #_parse-delimiter #'parse-unmatched-delimiter
   \} #_parse-delimiter #'parse-unmatched-delimiter
   \^ #'parse-sym-or-meta
   \@ #'parse-sym-or-deref
   \' #'parse-sym-or-quote
   \` #'parse-sym-or-syntax-quote
   \~ #'parse-sym-or-unquote
   \# #'parse-sym-or-dispatch
   :else #'parse-whitespace-number-or-symbol #_parse-bool-nil-or-sym #_parse-symbol})

(defn parse-leading [rdr]
  (let [ch (read-char rdr)
        ret (?read-comment rdr ch)]
    (if (identical? rdr ret)
      ch
      ret)))

(def ^:dynamic *macros* default-macros)
(def ^:dynamic *parse-leading* parse-leading #_skip-comments)

(defn parse
  ([reader] (parse reader nil nil {}))
  ([reader eof-error? sentinel]
   (parse reader eof-error? sentinel nil {}))
  ([reader eof-error? sentinel opts]
   (parse reader eof-error? sentinel nil opts))
  ([reader eof-error? sentinel return-on _opts]
   (try
     (loop []
       (let [ret (log-source
                  reader
                  (let [ch (*parse-leading* reader) #_(read-char reader)]
                    (cond
                      (nil? ch) (if eof-error? (err/throw-eof-error reader nil) sentinel)
                      (= ch return-on) tr/READ_FINISHED
                      (not (char? ch)) ch
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
    (code-node :klj children)))


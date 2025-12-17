(ns klj.nodes
  (:require [klj.reader :as klj]))

(defprotocol CoerceNode
  "Protocol to coerce values to syntax nodes."
  (as-node [expr]
    "Returns syntax node representing `expr`"))

(defn root-node [lang children]
  {:type :root
   :lang lang
   :children children})

;; TODO: Consider not passing in `rdr-fn`.
;; Instead, supply it when converting to an s-expr.
;; Use the reader on `:text` of any node that doesn't
;; have a `:expr` key.

(defn literal-node
  "Make a generic token. Could be used in a first pass to
  hold symbols, numbers, booleans, nil, and Clojure-style
  character literals."
  [src rdr-fn]
  {:type :literal
   :text src
   :expr rdr-fn})

(defn bool-node [src expr]
  {:type :bool
   :text src
   :expr expr})

(defn symbol-node [src s-ns s-name]
  {:type :symbol
   :text src
   :ns s-ns
   :name s-name
   :expr (symbol s-ns s-name)})

(defn keyword-node [src k-ns k-name]
  {:type :keyword
   :text src
   :ns k-ns
   :name k-name
   :expr (keyword k-ns k-name)})

(defn ws->str [ws]
  (cond
    (char? ws) ws
    (string? ws) ws
    :else (apply str ws)))

(comment
  (ws->str (vec "\n\n\r\n\r\r")))

(defn whitespace [ws]
  {:type :space
   :text (ws->str ws)})

(def sp (whitespace \space))

(def delimiters #{:opening :closing :neutral})

(defn delimiter [delim kind]
  {:type :delimiter
   :kind kind
   :text delim})

(def lparen (delimiter \( :opening))
(def rparen (delimiter \) :closing))
(def lbrace (delimiter \{ :opening))
(def rbrace (delimiter \} :closing))
(def lbrack (delimiter \[ :opening))
(def rbrack (delimiter \] :closing))

(defn punctuator [ch]
  {:type :punctuator
   :text ch})

(def comma (punctuator \,))
(def semi (punctuator \;))
(def colon (punctuator \:))
(def period (punctuator \.))

(defn eol-node
  "Vector of consecutive line endings with one element per ending.
  CRLF is a single ending with two characters."
  [eols]
  {:type :eol
   :eols eols
   :text (apply str (flatten eols))})

(def CRLF (eol-node [[\return \newline]]))
(def LF (eol-node [\newline]))
(def CR (eol-node [\return]))

(defn line-comment-node [open s eol-n]
  {:type :line-comment
   :open open
   :text s
   :eol eol-n})

(defn string-node [open close s]
  {:type :string
   :open open
   :close close
   :text s})

(comment
  (string-node \' \' "She said, \"Hi!\""))

(defn character-node [t ch]
  {:type :char
   :text t
   :expr ch})

(comment
  (character-node "'\n'" \newline))

(defn number-node [t n]
  {:type :number
   :text t
   :expr n})

(defn nil-node [txt]
  {:type :nil
   :text txt
   :expr nil})

(comment
  (nil-node))

(defn sequence-node [open close children]
  {:type :sequence
   :open open
   :close close
   :children children})

(defn as-nodes [coll]
  (mapv as-node coll))

(defn map-as-node [m]
  (let [nodes (->> m (map as-node) (interpose [comma sp]) flatten vec)]
    (sequence-node lbrace rbrace nodes)))

(extend-protocol CoerceNode
  nil
  (as-node [_] (nil-node))
  java.lang.Character
  (as-node [expr]
    (case expr
      \r CR
      \n LF
      \( lparen
      \) rparen
      \{ lbrace
      \} rbrace
      \[ lbrack
      \] rbrack
      \: colon
      \; semi
      \, comma
      (character-node (pr-str expr) expr)))
  java.lang.Long
  (as-node [expr]
    (number-node (pr-str expr) expr))
  java.lang.Double
  (as-node [expr]
    (number-node (pr-str expr) expr))
  java.lang.String
  (as-node [expr]
    (string-node \" \" (pr-str expr)))
  clojure.lang.PersistentArrayMap
  (as-node [expr]
    (map-as-node expr))
  clojure.lang.PersistentHashMap
  (as-node [expr]
    (map-as-node expr))
  clojure.lang.MapEntry
  (as-node [[k v]]
    [(as-node k) sp (as-node v)])
  clojure.lang.PersistentVector
  (as-node [expr]
    (sequence-node lbrack rbrack (interpose sp (as-nodes expr))))
  clojure.lang.PersistentList
  (as-node [expr]
    (sequence-node lparen rparen (interpose sp (as-nodes expr))))
  clojure.lang.Keyword
  (as-node [expr]
    (keyword-node (str expr) (namespace expr) (name expr)))
  clojure.lang.Symbol
  (as-node [expr]
    (symbol-node (str expr) (namespace expr) (name expr))))

(defn node->code [n]
  (case (:type n)
    :root (->> n :children (map node->code) (reduce str))
    :line-comment (str (:open n) (:text n) (node->code (:eol n)))
    :string (:text n) #_(str (:open n) (:text n) (:close n))
    :sequence (str (node->code (:open n)) #_(-> n :open :text)
                   (->> n :children (map node->code) (reduce str))
                   (node->code (:close n)) #_(-> n :close :text))
    :eol (apply str (flatten (:eols n)))
    (if-let [txt (:text n)]
      txt
      (str (pr-str n) \space))))


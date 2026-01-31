(ns klj.nodes
  (:require [klj.reader :as klj]
            #_[textasy.lines :refer [lines]])
  #?(:cljs (:import goog.string.StringBuffer)))

(defonce NONE (Object.))

(def ^:dynamic *none* NONE)

(defn none? [x]
  (identical? *none* x))

(defprotocol CoerceNode
  "Protocol to coerce values to syntax nodes."
  (as-node [expr]
    "Returns syntax node representing `expr`"))

(defn mk-node [typ & {:as opts}]
  (merge {::type typ} opts))

(defn parent-node [typ children & {:as opts}]
  (merge {::type typ :children children} opts))

(defn leaf-node [typ text & {:as opts}]
  (merge {::type typ :text text} opts))

(defn expr-node [typ text expr & {:as opts}]
  (merge {::type typ :text text :expr expr} opts))

(defn code-node
  ([children] (code-node :clj children))
  ([lang children]
   (parent-node :code children :lang lang)))

;; TODO: Consider not passing in var to reader function.
;; Instead, supply it when converting to an s-expr.
;; Use the reader on `:text` of any node that doesn't
;; have a `:expr` key.
;;
;; Things that have a known expression just use the `expr` key.
;; Things that have _no_ expression value, set it to `*none*`
;; Everything else is processed by *expr-reader*, perhaps `edn/read-string`
;; or `klj.reader/read-string`

(defn token-node
  "Make a generic token. Could be used in a first pass to
  hold symbols, numbers, booleans, nil, and Clojure-style
  character literals."
  [src expr-or-var]
  {::type :token
   :text src
   :expr #'klj/read-string #_expr-or-var})

(defn bool-node [src expr]
  (expr-node :bool src expr))

(defn symbol-node [src s-ns s-name]
  (expr-node :symbol src (symbol s-ns s-name) :ns s-ns :name s-name)
  #_{::type :symbol
   :text src
   :ns s-ns
   :name s-name
   :expr (symbol s-ns s-name)})

(defn symbolic-node
  ([src] (symbolic-node "##" src))
  ([open src]
   {::type :symbolic
    :open open
    :text src}))

(defn keyword-node [src k-ns k-name]
  {::type :keyword
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
  (leaf-node :space (ws->str ws))
  #_{::type :space
   :text (ws->str ws)})

(def sp (whitespace \space))

(def delimiters #{:opening :closing :neutral})

(defn delimiter [delim kind]
  (leaf-node :delimiter delim :kind kind)
  #_{::type :delimiter
   :kind kind
   :text delim})

(def lparen (delimiter \( :opening))
(def rparen (delimiter \) :closing))
(def lbrace (delimiter \{ :opening))
(def rbrace (delimiter \} :closing))
(def lbrack (delimiter \[ :opening))
(def rbrack (delimiter \] :closing))

(defn punctuator [ch]
  (leaf-node :punctuator ch)
  #_{::type :punctuator
   :text ch})

(def comma (punctuator \,))
(def semi (punctuator \;))
(def colon (punctuator \:))
(def period (punctuator \.))

(defn eol-node
  "Vector of consecutive line endings with one element per ending.
  CRLF is a single ending with two characters."
  [eols]
  (let [eols (if (sequential? eols) eols [eols])]
    (leaf-node :eol (apply str (flatten eols)) :eols eols)
    #_{::type :eol
     :eols eols
     :text (apply str (flatten eols))}))

(def CRLF (eol-node "\r\n" #_[[\return \newline]]))
(def LF (eol-node \newline))
(def CR (eol-node \return))

(defn line-comment-node
  "`close` should be a character, string, or nil"
  ([body]
   (line-comment-node ";;" \newline body))
  ([open [body eol]]
   (line-comment-node open eol body))
  ([open close body]
   {::type :line-comment
    :open open
    :body body
    :close close}))

(defn block-comment-node
  [open close body]
  {::type :block-comment
   :open open
   :body body
   :close close})

(defn regex-node
  ([s] (regex-node "#\"" \" s))
  ([open close s]
   {::type :regex
    :open open
    :close close
    :text s}))

(comment
  (regex-node "[a-z]+")
  (regex-node "#'" \' "She said, \"Hi!\""))

(defn string-node
  ([s] (string-node \" \" s))
  ([open close s]
   {::type :string
    :open open
    :close close
    :text s}))

(comment
  (string-node \' \' "She said, \"Hi!\""))

(defn character-node [t ch]
  (expr-node :char t ch)
  #_{::type :char
   :text t
   :expr ch})

(comment
  (character-node "'\n'" \newline))

(defn number-node [t n]
  (expr-node :number t n)
  #_{::type :number
   :text t
   :expr n})

(defn nil-node [txt]
  (expr-node :nil txt nil)
  #_{::type :nil
   :text txt
   :expr nil})

(comment
  (nil-node "Nada"))


(defn sequence-node [open close children]
  (parent-node :sequence children :open open :close close)
  #_{::type :sequence
   :open open
   :close close
   :children children})

(defn vector-node [children]
  (parent-node :vector children)
  #_{::type :vector
   :children children})

(defn list-node [children]
  (parent-node :list children)
  #_{::type :list
   :children children})

(defn map-node [children]
  (parent-node :map children)
  #_{::type :map
   :children children})

(defn set-node [children]
  (parent-node :set children)
  #_{::type :set
   :children children})

(defn deref-node [children]
  (parent-node :deref children))

(defn discard-node [children]
  (parent-node :discard children))

(defn eval-node [children]
  (parent-node :eval children))

(defn fn-node [children]
  (parent-node :fn children))

(defn meta-node
  ([children] (meta-node \^ children))
  ([open children]
   (parent-node :meta children :open open)))

(defn quote-node [children]
  (parent-node :quote children))

(defn syntax-quote-node [children]
  (parent-node :syntax-quote children))

(defn unquote-node [children]
  (parent-node :unquote children))

(defn unquote-splicing-node [children]
  (parent-node :unquote-splicing children))

(defn conditional-node [children]
  (parent-node :conditional children))

(defn conditional-splicing-node [children]
  (parent-node :conditional-splicing children))

(defn ns-map-node [children]
  (parent-node :ns-map children))

(defn tag-node [children]
  (parent-node :tag children))

(defn var-node [children]
  (parent-node :var children))

(defn as-nodes [coll]
  (mapv as-node coll))

(defn map-as-node [m]
  (let [nodes (->> m (map as-node) (interpose [comma sp]) flatten vec)]
    (map-node nodes)))

(extend-protocol CoerceNode
  nil
  (as-node [_] (nil-node "nil"))
  #?(:clj java.lang.Boolean :cljs boolean)
  (as-node [expr]
    (bool-node (pr-str expr) expr))
  ;; DMK TODO: May need to just coerce to character-node
  #?@(:clj (
  java.lang.Character
  #_(as-node [expr]
    (case expr
      \return CR
      \newline LF
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
  (as-node [expr]
    (character-node (pr-str expr) expr))))
  ;; java.lang.Long
  #?(:clj java.lang.Number :cljs number)
  (as-node [expr]
    (number-node (pr-str expr) expr))
  ;; java.lang.Double
  ;; (as-node [expr]
  ;;   (number-node (pr-str expr) expr))
  #?(:clj java.lang.String :cljs string)
  (as-node [expr]
    (string-node \" \" (let [s (pr-str expr)]
                         (subs s 1 (dec (count s)))) ))
  ;; #?(:clj java.util.regex.Pattern :cljs js/RegExp)
  ;; cljs maps:  PersistentHashMap PersistentArrayMap
  ;; #?(:clj clojure.lang.Var :cljs Var)
  #?(:clj clojure.lang.IPersistentMap :cljs PersistentHashMap)
  (as-node [expr]
    (map-as-node expr))
  #?@(:clj (clojure.lang.MapEntry
  (as-node [[k v]]
    [(as-node k) sp (as-node v)])))
  #?(:clj clojure.lang.IPersistentSet :cljs PersistentHashSet)
  (as-node [expr]
    (set-node (interpose sp (as-nodes expr))))
  #?(:clj clojure.lang.IPersistentVector :cljs PersistentVector)
  (as-node [expr]
    (vector-node (interpose sp (as-nodes expr))))
  #?(:clj clojure.lang.IPersistentList :cljs List)
  (as-node [expr]
    (list-node (interpose sp (as-nodes expr))))
  ;; #?(:clj clojure.lang.LazySeq :cljs LazySeq)
  ;; #?(:clj clojure.lang.Cons :cljs Cons)
  ;; cljs EmptyList is special
  #?(:clj clojure.lang.Keyword :cljs Keyword)
  (as-node [expr]
    (keyword-node (str expr) (namespace expr) (name expr)))
  #?(:clj clojure.lang.Symbol :cljs Symbol)
  (as-node [expr]
    (symbol-node (str expr) (namespace expr) (name expr))))

;; These next two functions are modeled after those in `parcera.core`
;; and adapted to work with the map-based nodes of `klj`.

(defn code*
  "internal function used to imperatively build up the code from the provided
   AST as Clojure's str would be too slow"
  [ast #?(:clj ^StringBuilder sb :cljs ^StringBuffer sb)]
  (case (::type ast)
    :code
    (run! #(code* % sb) (:children ast))
    :block-comment
    (.. sb (append (:open ast)) (append (:body ast)) (append (:close ast)))
    :line-comment
    (.. sb (append (:open ast)) (append (:body ast)) (append (:close ast)))
    :sequence
    (do (code* (:open ast) sb)
        (run! #(code* % sb) (:children ast))
        (code* (:close ast) sb))
    :list
    (do (. sb (append "("))
        (run! #(code* % sb) (:children ast))
        (. sb (append ")")))
    :map
    (do (. sb (append "{"))
        (run! #(code* % sb) (:children ast))
        (. sb (append "}")))
    :set
    (do (. sb (append "#{"))
        (run! #(code* % sb) (:children ast))
        (. sb (append "}")))
    :vector
    (do (. sb (append "["))
        (run! #(code* % sb) (:children ast))
        (. sb (append "]")))
    :fn
    (do (. sb (append "#"))
        (run! #(code* % sb) (:children ast)))
    :uneval
    (do (. sb (append "#_"))
        (run! #(code* % sb) (:children ast)))
    :string
    (.. sb (append (:open ast)) (append (:text ast)) (append (:close ast)))
    :symbolic
    (.. sb (append (:open ast)) (append (:text ast)))
    (:bool :char :delimiter :eol :keyword :macro_keyword :nil :number :punctuator :space :symbol :token)
    (. sb (append (:text ast)))
    #_else (. sb (append (str ast)))))

(defn code
  "Transforms your AST back into code

   ast: The nested sequence of [:keyword & content] which MUST follow the
        same structure as the result of `(parcera/clojure input-string)`

   Returns a string representation of the provided AST

   In general (= input (parcera/code (parcera/clojure input)))"
  [ast]
  (let [sb #?(:clj (new StringBuilder)
              :cljs (new StringBuffer))]
    (code* ast sb)
    (. sb (toString))))

(declare exprs)

(defn expr [n]
  (case (::type n)
    :code (let [es (exprs (:children n))]
            (if (next es)
              (list* 'do es)
              (first es)))
    ;; TODO: think about handling raw string data, vs regular strings.
    :string (klj/read-string (str (:open n) (:text n) (:close n)))
    :token (let [expr* (:expr n)]
             (if (var? expr*) (expr* (:text n)) expr*))
    ;; probably need to filter these as you go
    (:comment :delimiter :eol :space :punctuator :uneval) *none*
    (:bool :char :keyword :nil :number :symbol) (:expr n)
    :symbolic (klj/read-string (str (:open n) (:text n)))
    :vector (vec (exprs (:children n)))
    :list (apply list (exprs (:children n)))
    :set (set (exprs (:children n)))
    :map (apply hash-map (exprs (:children n)))
    :sequence
    (let [open (:open n)
          open (cond
                 (map? open) (-> open :text str)
                 (string? open) open
                 (char? open) (str open))]
      (case open
        "(" (apply list (exprs (:children n)))
        "[" (vec (exprs (:children n)))
        "#{" (set (exprs (:children n)))
        "{" (apply hash-map (exprs (:children n)))))
    n))

(defn exprs [nodes]
  (->> nodes (map expr) (remove #(identical? % *none*))))

(defn node->code [n]
  (case (::type n)
    :code (->> n :children (map node->code) (reduce str))
    :line-comment (str (:open n) (:text n) (node->code (:eol n)))
    :string (:text n) #_(str (:open n) (:text n) (:close n))
    :sequence (str (node->code (:open n)) #_(-> n :open :text)
                   (->> n :children (map node->code) (reduce str))
                   (node->code (:close n)) #_(-> n :close :text))
    :eol (apply str (flatten (:eols n)))
    (if-let [txt (:text n)]
      txt
      (str (pr-str n) \space))))


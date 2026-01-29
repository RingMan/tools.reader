(ns klj.blocks
  "Helper functions for reading blocks of text including
   strings, raw strings, line comments, and block comments"
  (:require
   #?@(:clj
       [[clojure.tools.reader :as tr]
        [clojure.tools.reader.impl.errors :as err]
        [clojure.tools.reader.reader-types :refer
         [peek-char read-char unread]]
        [klj.chars :refer [newline-or-nil?]]]
       :cljs
       [[cljs.tools.reader :as tr]
        [cljs.tools.reader.impl.errors :as err]
        [clojure.tools.reader.reader-types :refer
         [peek-char read-char unread]]]))
  #?(:clj (:import (java.lang StringBuffer StringBuilder))))

(defn chained-reader-fn
  "Accepts two read functions that process the same macro character.
  Returns result of `first-fn` it returns the reader; otherwise, the
  result of `next-fn`."
  [first-fn next-fn]
  (fn [rdr ch]
    (let [ret (first-fn rdr ch)]
      (if (identical? rdr ret)
        (next-fn rdr ch)
        ret))))

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

(defn read-n
  "Call the given function on the given reader until `n` values matching `p?` have been
   collected."
  [#?(:cljs ^not-native reader :default reader) node-tag read-fn p? n]
  {:pre [(pos? n)]}
  (loop [c 0
         vs []]
    (if (< c n)
      (if-let [v (read-fn reader)]
        (recur
         (if (p? v) (inc c) c)
         (conj vs v))
        (err/throw-eof-reading reader node-tag (str " node expects " n " value(s)")))
      vs)))

(defn read-to-eol
  "Advances the reader to the end of a line.
  Supports CR, LF, or CRLF line endings.
  Returns the line, including the EOL sequence."
  [reader]
  (let [buf (StringBuffer.)]
    (loop [c (read-char reader) eol nil]
      (case c
        nil [(str buf) eol]
        \newline [(str buf) (str eol c)]
        \return (if (newline-or-nil? (peek-char reader))
                  (recur (read-char reader) c)
                  [(str buf) c])
        (do (.append buf c) (recur (read-char reader) eol))))))

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

(defn read-nested-comment
  "Assumes opening `lch` and `mch` have been read.
  Returns a map with keys `:open`, `:close` and `:comment`.
  Comments can be nested arbitrarily deep.
  Reader is positioned at character just after the comment's close."
  [rdr lch mch rch]
  (let [buf (StringBuffer.)]
    (loop [state :reading, level 1]
      (let [ch (read-char rdr)]
        (when (pos? level) (.append buf ch))
        ;; when see closing `rch` return if level 0, else recur with (dec level)
        (cond
          (nil? ch) (err/throw-eof-error rdr nil)
          (zero? level) (do
                          (unread rdr ch)
                          {:open (str lch mch)
                           :close (str mch rch)
                           :comment (.substring buf 0 (- (.length buf) 2))})
          (and (= state :opening) (identical? ch mch))
          (recur :reading (inc level))
          (and (= state :closing) (identical? ch rch))
          (recur :reading (dec level))
          (identical? ch lch) (recur :opening level)
          (identical? ch mch) (recur :closing level)
          :else (recur :reading level))))))

(defn line-comment-reader
  ;; Returns a fn suitable as a reader-macro
  ([ch1]
   (fn [rdr ch]
     (if (identical? ch ch1)
       (let [[txt eol] (read-to-eol rdr)]
         {:type :comment
          :open ch
          :comment txt
          :eol eol})
       rdr)))
  ;; Returns a function suitable as first arg to chained-reader-fn
  ([ch1 ch2]
   (fn [rdr ch]
     (if (and (identical? ch ch1) (identical? (peek-char rdr) ch2))
       (let [[txt eol] (do (read-char rdr) (read-to-eol rdr))]
         {:type :comment
          :open (str ch ch2)
          :comment txt
          :eol eol})
       rdr))))

(defn ?read-cpp-comment [rdr ch]
  (if (identical? ch \/)
    (case (peek-char rdr)
      \/ (do (read-to-eol rdr) {:open "//"
                                :comment "cpp style"
                                :eol \newline})
      \* {:open "/*" :close "*/" :comment (read-to-suffix rdr "*/")}
      rdr)
    rdr))

(defn ?read-comment
  "If first char is possible start of comment, tries to read
  line or block comment based on peeking at next char. Returns
  either the comment node or the reader if there is none."
  [reader ch]
  (case ch
    \# (case (peek-char reader)
         \space {:open "# "
                 :comment (do (read-char reader)
                              (read-to-eol reader))
                 :eol \newline}
         \| {:open "#|" :close "|#" :comment (do (read-char reader)
                                                 (read-to-suffix reader "|#"))}
         reader)
    \/ (case (peek-char reader)
         \/ {:open "//"
             :comment (do (read-char reader)
                          (read-to-eol reader))
             :eol \newline}
         \* {:open "/*" :close "*/" :comment (do (read-char reader)
                                                 (read-to-suffix reader "*/"))}
         reader)
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
           {:open (str ch ch2)
            :close suffix
            :comment (do (read-char reader)
                         (read-to-suffix reader suffix))}
           reader))
    reader))

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

(defn read-comment
  [reader _initch _opts _pending-forms]
  ;; (println "dmk raw read-comment")
  (if (not= \\ (peek-char reader))
    (read-to-eol reader)
    (do
      (read-char reader) ;; skip \\
      (if (= \R (peek-char reader))
        (read-raw-block reader \( \) \;)
        (read-to-eol reader)))))

;; TODO: consider parameterizing treatment of consecutive quotes
;; Also, we don't actually want to escape characters. We just
;; need the string data. Look at `rewrite-clj.parser.impl/read-string-data`
;; for comparison. We just need to guard against an escaped double quote.

(defn read-delimited-string
  [reader quote-ch]
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


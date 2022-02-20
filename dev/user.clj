(ns user
  (:require [clojure.tools.reader.reader-types :as rt]))

(defprotocol Greeter
  (say-hi [greeter])
  (say-bye [greeter]))

(deftype MyGreeter
  [ent]
  Greeter
  (say-hi [_] (str "Howdy, " ent)))

(comment
(def g (->MyGreeter "Dave"))

(def g2 (MyGreeter. "man"))

(say-hi g)
(say-hi g2)
)

(comment
"
Currently, the row and column of an IndexingPushbackReader are for the _next_
char that will be read. It goes with `peek-char`.
\return is transformed to \newline and normalize? is set to true
The subsequent \newline is discarded.
However, peek-char still returns the actual next char

Desired invariants:
 1) (= (peek-char rdr) (read-char rdr))
 2) If you only unread the very character you just read, different sequences
    of interleaved reads and unreads should produce the same result as just all
    reads
 2) row and col of each character should be monotonically increasing

The first two invariants are not true for the current reader.
"
 (let [[row col]
       (case ch
         ;after \newline or \formfeed, col is always one
         ;inc row if previous column wasn't zero (prev ch was \return)
         (\newline \formfeed) [(if (zero? col) row (inc row)) 1]
         ;after \return
         ;  inc row,
         ;  set col to zero if followed by \newline or \formfeed
         \return [(inc row) (if (#{\newline \formfeed} (peek-char rdr)) 0 1)])])
\h \i \return \newline \b \y \e
\^
)

(defprotocol EolNormalizingReader
  "Marker type for text readers that normalize line endings")

; An EolNormalizingReader that normalizes line endings to Linux style.

(deftype LinuxNormalizingReader
  [rdr]
  EolNormalizingReader
  rt/Reader
  (read-char [reader]
    (let [ch (rt/read-char rdr)]
      (cond
        (not (identical? ch \return)) ch
        (identical? (rt/peek-char rdr) \newline) (rt/read-char rdr)
        :else \newline)))
  (peek-char [reader]
    (let [ch (rt/peek-char rdr)]
      (if-not (identical? ch \return)
        ch
        \newline)))
  rt/IPushbackReader
  (unread [reader ch]
    (if-not (identical? ch \return)
      (rt/unread rdr ch)
      (throw (ex-info "Can't unread a carriage return"
                      {:msg "Illegal character in call to unread"
                       :ch \return}))))
  java.io.Closeable
  (close [reader]
    (when (instance? java.io.Closeable rdr)
      (.close ^java.io.Closeable rdr))))

(comment
  (def s "1\r2\r\n3\n4\r")
  (def r (->LinuxNormalizingReader (rt/to-pbr s 2)))
  (rt/read-char r)
  (rt/peek-char r)
  (rt/unread r \newline)
  (try (rt/unread r \newline)
       (catch clojure.lang.ExceptionInfo e
         (ex-data e))))

(defn read-ch [r]
  (let [col (rt/get-column-number r)
        line (rt/get-line-number r)
        ch (rt/read-char r)
        pk (rt/peek-char r)]
    [[line col] ch pk]))

(defn peek-ch [r]
  (let [pk (rt/peek-char r)
        col (rt/get-column-number r)
        line (rt/get-line-number r)]
    [[line col] pk]))

(defn unread-ch [r ch]
  (let [ch (rt/unread r ch)
        pk (rt/peek-char r)
        col (rt/get-column-number r)
        line (rt/get-line-number r)]
    [[line col] ch pk]))

(comment
  (require '[clojure.tools.reader.reader-types :as rt] :reload)

  (def s "1A\r2Bb\n3C\r\f4Dd\r\n5E\n")
  #_(def s "123\n456\n789\n123\n456\n789")
  (def nr (->LinuxNormalizingReader (rt/to-pbr s 10)))
  (def r (rt/indexing-push-back-reader nr 10))
  (def v (atom []))
  (let [x (read-ch r)]
    (swap! v conj x))
  (let [[[_ _] ch _] (last @v)]
    (unread-ch r ch)
    (swap! v pop))

  (read-ch r)
  (peek-ch r)
  (unread-ch r \E)
  (def n 4)
  (for [_ (range n)]
    (read-ch r))
  (for [ch (reverse (take n s))]
    (unread-ch r ch)))

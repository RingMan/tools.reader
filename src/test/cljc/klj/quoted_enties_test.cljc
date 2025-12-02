(ns klj.quoted-entities-test
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *data-readers*])
  (:use [klj.reader :only [read read-string] :as klj]
        [clojure.tools.reader :only [*default-data-reader-fn* *data-readers*]]
        [clojure.tools.reader.reader-types :only [string-push-back-reader
                                                  indexing-push-back-reader]]
        [clojure.test :only [deftest is are testing]]
        [clojure.tools.reader.impl.utils :exclude [char]])
  (:require [clojure.tools.reader.edn :as tre])
  (:import clojure.lang.BigInt
           (java.io StringReader BufferedReader)
           clojure.lang.LineNumberingPushbackReader))

;NOTE: This test requires read support for raw strings
(deftest read-quoted-kwd
  (testing "Suffix string with `k` or `:` to make a quoted keyword"
    (are [x y] (= (keyword x)
                  (read-string (str \" y \" \k))
                  (read-string (str \" y \" \:)))
         "kwd with space" "kwd with space"
         "kwd-\\bslash\ttab\newline\fformfeed" "\R(kwd-\\bslash\ttab\newline\fformfeed)"
         "kwd-\\bslash\ttab\newline\fformfeed" "kwd-\\\\bslash\\\ttab\\\newline\\\fformfeed"
         "kwd (with) spaces {and} [delims]" "kwd (with) spaces {and} [delims]")))


;NOTE: This test requires read support for raw strings
(deftest read-quoted-sym
  (testing "Suffix string with `s` or `~` to make a quoted symbol"
    (are [x y] (= (symbol x)
                  (read-string (str \" y \" \s))
                  (read-string (str \" y \" \~)))
         "sym with space" "sym with space"
         "sym-\\bslash\ttab\newline\fformfeed" "\R(sym-\\bslash\ttab\newline\fformfeed)"
         "sym-\\bslash\ttab\newline\fformfeed" "sym-\\\\bslash\\\ttab\\\newline\\\fformfeed"
         "sym (with) spaces {and} [delims]" "sym (with) spaces {and} [delims]")))

;NOTE: This test requires read support for raw strings
(deftest read-quoted-char
  (testing "Suffix string with `c` to make a quoted char"
    (are [x y] (= x (read-string y))
         \backspace "\R("\b"c)"
         \formfeed  "\R("\f"c)"
         \newline   "\R("\n"c)"
         \return    "\R("\r"c)"
         \space     "\R("\s"c)"
         \tab       "\R("\t"c)"
         \\         "\R("\\"c)"
         \"         "\R("\""c)"
         \a         "\R("a"c)"
         ;using plain instead of raw strings
         \backspace "\"\\b\"c"
         \newline   "\"\\n\"c")))


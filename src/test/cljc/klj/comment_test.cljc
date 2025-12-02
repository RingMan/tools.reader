(ns klj.comment-test
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

(deftest read-line-comments
  (are [x] (true? (read-string x))
       ";line comment\ntrue"
       ";; line comment\ntrue"
       "//cpp-style line comment\ntrue"
       "// cpp-style line comment\ntrue"
       "# shell-style line comment\ntrue")
  (testing "Semicolon followed by space or EOF is _not_ a comment"
    (are [x] (= (symbol ";") (read-string x))
         "; "
         ";")))

(deftest read-block-comments
  (testing "Semicolon followed by opening delimiter introduces block comment"
    (are [x] (= true (read-string x))
         ";(block comment); true"
         ";[block comment]; true"
         ";{block comment}; true"))
  (testing "Semicolon followed by other paired characters are also block comments"
    (are [x] (= true (read-string x))
         ";<block comment>; true"
         ";\"block comment\"; true"
         ";'block comment'; true"
         ";|block comment|; true"))
  (testing "Semicolon followed by non-space, non-delimiter is line comment"
    (are [x] (true? (read-string x))
         ";line comment\ntrue"
         ";; line comment\ntrue"))
  (testing "C-style block comment"
    (is (true? (read-string "/* c-style block comment */ true"))))
  (testing "Block comments can nest"
    (are [x] (true? (read-string x))
         ";[level one ;(level ;{level three}; two);]; true"
         "/* c-style ;[ klj-style ]; */ true")))

;; TODO: finish test. Add nested raw comments
(deftest read-raw-comments
  (testing "FIXME: Semicolon followed by \\R starts a raw comment"
    (are [x] (true? (read-string x))
         "\R_(;\R(my raw comment); true)_"
         "\R_(;\Rklj(my delimited raw comment)klj; true)_")))

(comment
  (read-string "" [1 2 3]))

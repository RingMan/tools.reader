(ns klj.semicolon-test
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

(def ^:private semi (symbol ";"))

(deftest read-semicolon
  (testing "Semicolon followed by space, closing delimiter or EOF is a symbol"
    (are [x y] (= x (read-string y))
         semi "; "
         semi ";"
         [semi] "[;]"
         (list semi) "(;)"
         {:semi semi} "{:semi ;}"))
  (testing "Semicolon followed by non-space, non-delimiter is line comment"
    (are [x] (true? (read-string x))
         ";line comment\ntrue"
         ";; line comment\ntrue"))
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
  (testing "Semicolon _inside_ a symbol is part of the symbol"
    (let [in "a;b"]
      (are [in] (= (symbol in) (read-string in))
           "a;b"
           "a;b;c;;d;;;e")))
  (testing "Semicolon at _start_ of symbol must be escaped"
    (are [x y] (= x (read-string y))
         (symbol ";a") "\\;a"
         (symbol ";;") "\\;;;"))
  (testing "Semicolon just _after_ a form is a separate symbol"
    (are [x] (let [s (pr-str x)]
               (= [x semi] (try
                             (read-string (str \[ s \; \]))
                             (catch Exception e :error))))
         ##Inf ##-Inf [] () {} #{}
         \tab \newline \a \;
         true false nil 42 "hi" :kwd)
    (let [[a b] (read-string "[##NaN;]")]
      (is (Double/isNaN a))
      (is (= semi b)))
    (is (= ['sym semi] (read-string "[sym;]")))))


(ns klj.symbol-test
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

(deftest read-em-all
  (let [in "\R([#\0 #\])"](is true)))

(deftest read-specials
  (let [t (symbol (str \t "rue"))
        f #_(symbol "false") (symbol (str \f "alse"))
        n #_(symbol "nil") (symbol (str \n "il"))]
    ;; Use escape mechanism
    (is (= n (read-string "\\nil")))
    (is (= f (read-string "\\false")))
    (is (= t (read-string "\\true")))
    ;; Use quoted symbols
    (is (= n (read-string "\"nil\"~")))
    (is (= f (read-string "\"false\"~")))
    (is (= t (read-string "\"true\"~")))
    ))

(comment
  (read-string "\\#withstuff")
  (read-string "\"\\x20abc\"c"))
;; Use the new #\ dispatch for one-character symbols that would be
;; taken for digits, character literals, macro characters or delimiters.

(deftest read-single-chars-as-symbols
  (let [digits (range 10)
        signed-nums ["-42" "+42"]
        octal [\o123 \o377]
        macro-chs [\" \: \' \@ \^ \` \~ \\ \#]
        delims [\( \) \{ \} \[ \]]
        first-chars (concat digits signed-nums octal macro-chs delims)]
    (doseq [ch first-chars]
      ;; using quoted symbols
      #_(is (= (symbol (str ch)) (read-string (str \" ch \" \~))))
      ;; using new `#\` dispatch
      (is (= (symbol (str ch)) (read-string (str \# \\ ch)))))))

(deftest hash-as-symbol
  (testing "Hash followed by closing delimiter is a symbol"
    (are [f in] (= (symbol "#") (f (read-string in)))
         first "[#]"
         :hash "{:hash #}"
         first "#{#}"
         first "(#)")))

(deftest read-single-escape-char-as-symbols-orig
  (let [escapes [[\backspace \f] [\formfeed \f] [\newline \n] [\return \r] [\space \s] [\tab \t]]]
    (doseq [[esc-name esc-char] escapes]
      (is (symbol (str esc-name)) (read-string (str \# \\ esc-char))))))

(deftest read-single-escape-char-as-symbols
  (are [x y] (= (symbol (str x)) (read-string (str \# \\ y)))
       \backspace \b
       \formfeed \f
       \newline \n
       \return \r
       \space \s
       \tab \t
       \o040 "o040"
       \u0020 "u0020"))

(deftest read-symbolic-values-as-symbols
  (testing "Escape first `#` to read symbolic value as a symbol"
    (are [x] (= (symbol x) (read-string (str \\ x)))
         "##Inf"
         "##-Inf"
         "##NaN")))

(comment
  (ns-unmap *ns* 'read-named-numbers-as-symbols)
  (read-string "[1; :one; true; \"hi\";]")
  (read-string (str \\ \\ "newline"))
  (read-string "##Inf")
  (read-string (str \# \\ \s)))


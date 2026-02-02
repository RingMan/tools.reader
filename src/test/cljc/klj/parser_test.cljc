(ns klj.parser-test
  (:require [klj.generate :as g]
            [klj.nodes :as n]
            [klj.parser :as p]
            [klj.reader :as r]
            [com.gfredericks.test.chuck.clojure-test :refer [for-all] :as chuck]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]))

(defn parse-str [s]
  (let [rdr (r/string-reader s)]
    (p/parse rdr)))

(defn parse-str-all [s]
  (let [rdr (r/string-reader s)]
    (p/parse-all rdr)))

;; TODO: add regex here too, but need a generator of some sort

(def simple-vals*
  {:number [g/number :number number?]
   :boolean [g/bool :bool boolean?]
   :nil [g/nil-g :nil nil?]
   :character [g/char :char char?]
   :string [g/string-ascii :string string?]
   :keyword [g/keyword :keyword keyword?]
   :symbol [g/symbol :symbol symbol?]
   :symbolic [g/symbolic-val :symbolic number?]})

(def simple-val-type
  (gen/elements (vec (keys simple-vals*))))

(def simple-val
  (gen/let [vt simple-val-type]
    (let [[vg tok-typ pred] (simple-vals* vt)]
      [(gen/generate vg) tok-typ pred])))

(comment
  *e
  (n/code (parse-str "##PI"))
  (string? (n/expr (parse-str "\"hi\\n \\\"there\\\" bye\""))))

(defspec t-simple-values
  (for-all [[in tok-typ pred] simple-val]
           (let [rdr (r/string-reader in)
                 n (p/parse rdr)]
             (is (= (::n/type n) tok-typ))
             (is (pred (n/expr n)))
             (is (= (n/code n) in)))))

(comment (t-simple-values))

;; TODO: proabably remove this as `t-simple-values` does the job
(defspec t-numbers-parse-as-number-node
  (for-all [in g/number]
           (let [rdr (r/string-reader in)
                 n (p/parse rdr)]
             (is (= (::n/type n) :number))
             (is (= (:text n) in))
             (is (number? (n/expr n)))
             (is (= (n/code n) in)))))

(defspec t-kwd-with-trailing-sep
  (for-all [[in sep] (gen/tuple g/keyword (gen/elements [\, \; \:]))]
           (let [rdr (r/string-reader (str in sep))
                 n (p/parse rdr)]
             (is (= (::n/type n) :keyword))
             (is (= (:text n) in))
             (is (= (n/code n) in)))))

(defspec t-sym-with-trailing-sep
  (for-all [[in sep] (gen/tuple g/symbol (gen/elements [\, \; \:]))]
           (let [rdr (r/string-reader (str in sep))
                 n (p/parse rdr)]
             (is (= (::n/type n) :symbol))
             (is (= (:text n) in))
             (is (= (n/code n) in)))))

(comment (t-sym-with-trailing-sep))

(defspec t-block-comments
  (for-all [bc g/block-comment]
           (let [in (g/block-comment-str bc)
                 rdr (r/string-reader in)
                 n (p/parse rdr)]
             (is (= (::n/type n) :block-comment))
             (is (= (:open bc) (:open n)))
             (is (= (:close bc) (:close n)))
             (is (= (n/code n) in)))))

(defspec t-line-comments
  (for-all [lc g/any-line-comment]
           (let [in (g/line-comment-str lc)
                 rdr (r/string-reader in)
                 n (p/parse rdr)]
             (is (= (::n/type n) :line-comment))
             (is (= (:open lc) (:open n)))
             (is (= (:close lc) (str (:close n))))
             (is (= (n/code n) in)))))

(comment
  (t-block-comments)
  (t-line-comments)
  (let [c (gen/generate g/any-line-comment)
        in (g/line-comment-str c)
        n (parse-str-all "/**/# \r;**;//\n;[];")
        out (n/code n)]
    {:in in :n n :out out}))


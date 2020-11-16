(ns clojure.tools.reader-types-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.tools.reader.reader-types :as rt]))

;; The property below should be true for all reader types.
;; It fails for the IndexingPushbackReader and also the
;; SourceLoggingPushbackReader.

(defspec peek-equals-read 100
  (prop/for-all [s gen/string]
    (let [rdr (rt/indexing-push-back-reader s)
          pairs (loop [ret []]
                  (if (rt/peek-char rdr)
                    (recur (conj ret [(rt/peek-char rdr) (rt/read-char rdr)]))
                    ret))]
      (every? (fn [[peek read]] (= peek read)) pairs))))


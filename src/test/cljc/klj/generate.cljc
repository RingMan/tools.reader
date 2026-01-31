(ns klj.generate
  (:refer-clojure :exclude [char keyword symbol])
  (:require [clojure.string :as str]
            [clojure.test.check.generators :as gen]))

;; NOTE: Most of this could probably move to textasy project.

;; Things to generate
;; - line and block comments
;; - strings
;;   - double quoted
;;   - single quoted
;;   - raw
;;   - single and multi-line
;; - EOL sequences
;; - whitespace sequences
;; - chars
;; - numbers
;; - keywords
;; - symbols
;; - bools
;; - nil
;;
;; Sequences
;; - list
;; - map
;; - set
;; - vector

(comment
  (let [xs (gen/sample gen/simple-type)])
  (-> gen/char gen/generate ((juxt int identity)))
  \u000e
  :eoc)

;;; The first few generators return a string value

(def blank (gen/return ""))

(defn optional
  ([g]
   (gen/one-of [g blank]))
  ([g freq]
   (gen/frequency [[freq g] [(- 100 freq) blank]])))

(defn join
  "Returns a generator that joins the results of the given generators"
  [& generators]
  (gen/fmap
    str/join
    (apply gen/tuple generators)))

(def comma (gen/return ", "))

(defn commas [min-n max-n]
  (gen/let [n (gen/choose min-n max-n)]
    (reduce str (repeat n \,))))

(def space (gen/return " "))

(defn spaces [min-n max-n]
  (gen/let [n (gen/choose min-n max-n)]
    (reduce str (repeat n \space))))

(def tab (gen/return "\t"))

(defn tabs [min-n max-n]
  (gen/let [n (gen/choose min-n max-n)]
    (apply str (repeat n \tab))))

(def eol
  (gen/elements ["\n" "\r" "\r\n"]))

(def optional-space
  (optional space))

(def spaces-or-tabs
  (gen/fmap
    (partial apply str)
    (gen/vector (gen/elements [\space \tab]) 1 5)))

(def opt-spaces-or-tabs
  (optional spaces-or-tabs 80))

(comment
  (gen/sample opt-spaces-or-tabs))

(def separator
  (gen/one-of [comma space tab eol]))

(defn separators [min-n max-n]
  (gen/fmap
    (partial apply str)
    (gen/vector separator min-n max-n)))

;; NOTE: It might be nice to guarantee no leading or trailing whitespace

(def inline-text
  (gen/such-that
    #(re-matches #"[^\r\n]*" %)
    gen/string-ascii))

(def inline-text-non-empty
  (gen/not-empty inline-text))

(defn text-span [lead-g trail-g span-g]
  (gen/fmap
    (fn [[lead span trail]]
      (str lead span trail))
    (gen/tuple lead-g span-g trail-g)))

(def bool
  (gen/elements ["true" "false"]))

(def nil-g (gen/return "nil"))

(defn pr-generator [g]
  (gen/fmap pr-str g))

(def named-char
  (gen/elements ["\\newline" "\\space" "\\tab" "\\backspace" "\\formfeed" "\\return"]))

(def char (gen/frequency [[8 (pr-generator gen/char-ascii)] [2 named-char]]))
(def small-int (pr-generator gen/small-integer))
(def large-int (pr-generator gen/large-integer))
(def big-int
  (gen/fmap #(str % \N) large-int))
(def string-ascii (pr-generator gen/string-ascii))
(def keyword (pr-generator gen/keyword))
(def symbol (pr-generator gen/symbol))
(def ratio (pr-generator gen/ratio))
(defn double* [opts]
  (pr-generator (gen/double* (merge {:infinite? false :NaN? false} opts))))

(def symbolic-val
  (gen/elements ["##Inf" "##-Inf" "##+Inf" "##PI" "##E"]))

(def number
  (gen/one-of [small-int large-int big-int (double* nil) ratio]))

(def simple-val
  (gen/one-of [bool char keyword nil-g small-int ratio (double* nil)
               string-ascii symbol symbolic-val]))

(comment
  (gen/sample char)
  (gen/sample big-int 30)
  (doseq [x (gen/sample simple-val 20)]
    (println x))
  (gen/sample
    (text-span opt-spaces-or-tabs blank inline-text-non-empty))
  :eoc)

;;; Various line generators for single and multiple lines

;; Single line generators return a map with `:line` and `:eol` keys.
;; Multi-line generators return a vector of the line maps.

(defn blank-line [space-g eol-g]
  (gen/fmap
    (fn [[space eol]]
      {:line space :eol eol})
    (gen/tuple space-g eol-g)))

(def empty-line (partial blank-line blank))

(defn blank-lines [space-g min-n max-n eol-g]
  (gen/let [n (gen/choose min-n max-n)
            spaces (gen/vector space-g n)
            eols (gen/vector eol-g (dec n))
            opt-eol (optional eol-g)]
    (mapv (fn [space eol]
            {:line space :eol eol})
          spaces (conj eols opt-eol))
    #_(str/join (interleave leads lines trails (conj eols opt-eol)))))

(def empty-lines (partial blank-lines blank))

(comment
  (gen/sample (empty-lines 2 3 eol))
  (gen/sample (empty-line eol)))

(def line-comment-prefix
  (gen/elements [#_";;" "#!" "//" "# " #_"--"]))

(defn line [span-g eol-g]
  (gen/fmap
    (fn [[line eol]] {:line line :eol eol})
    (gen/tuple span-g eol-g)))

(defn lines [min-n max-n span-g eol-g]
  (gen/let [n (gen/choose min-n max-n)
            spans (gen/vector span-g n)
            eols (gen/vector eol-g (dec n))
            opt-eol (optional eol-g)]
    (mapv (fn [line  eol] {:line line :eol eol})
          spans (conj eols opt-eol))))

(comment
  (let [span-g (text-span spaces-or-tabs blank inline-text-non-empty)
        line (gen/generate (line span-g eol))
        lines (gen/generate (lines 2 3 span-g eol))]
    {:line line
     :lines lines})
  :eoc)

;; TODO: probably delete this.
;; Use `lines` instead
(defn multi-line-text-g [min-n max-n lead-g line-g trail-g eol-g]
  (gen/let [n (gen/choose min-n max-n)
            leads (gen/vector lead-g n)
            lines (gen/vector line-g n)
            trails (gen/vector trail-g n)
            eols (gen/vector eol-g (dec n))
            opt-eol (optional eol-g)]
    #_{:n n :lines lines :eols eols}
    (mapv (fn [lead line trail eol]
            {:line (str lead line trail) :eol eol})
          leads lines trails (conj eols opt-eol))
    #_(str/join (interleave leads lines trails (conj eols opt-eol)))))

(comment
  (multi-line-text-g 2 4 blank inline-text-non-empty blank eol)
  (let [ml-text (multi-line-text-g
                  1 2
                  opt-spaces-or-tabs
                  inline-text-non-empty
                  opt-spaces-or-tabs
                  eol)]
    (gen/generate ml-text))
  :eoc)

(defn delimited-values
  "Generates a body delimited by generated opening and closing pair."
  [open-close-pair-g values-g]
  (gen/fmap
    (fn [[[open close] values]]
      {:open open :children values :close close})
    (gen/tuple open-close-pair-g values-g)))

(defn separated-values [min-n max-n value-g sep-g]
  (gen/let [n (gen/choose min-n max-n)
            values (gen/vector value-g n)
            seps (gen/vector sep-g (dec n))
            opt-sep (optional sep-g)]
    (let [vs (vec (interleave values (conj seps opt-sep)))]
      (if-not (empty? opt-sep) vs (vec (butlast vs))))))

(comment
  (gen/generate
    (separated-values
      1 5
      simple-val
      separator))
  (gen/generate
    (delimited-values
      (gen/elements [["(" ")"] ["[" "]"] ["{" "}"]])
      (separated-values 4 4 simple-val separator))))

(defn delimited-lines
  "Generates a body delimited by generated opening and closing pair."
  [open-close-pair-g body-g]
  (gen/fmap
    (fn [[[open close] body]]
      {:open open :body body :close close})
    (gen/tuple open-close-pair-g body-g)))

(comment
  (gen/sample (optional eol 80)))

(defn line-comment-generator [prefix-g text-g eol-g]
  (gen/fmap
    (fn [[prefix text eol]]
      {:open prefix :body text :close eol}
      #_(str prefix text eol))
    (gen/tuple
      prefix-g
      text-g
      eol-g)))

(defn line-comment-str [{:keys [open body close]}]
  (str open body close))

(def clj-line-comment
  (line-comment-generator (gen/return ";") inline-text eol))

(def any-line-comment
  (line-comment-generator
    line-comment-prefix
    (join optional-space inline-text)
    eol))

(comment
  (gen/sample any-line-comment)
  (gen/sample
    (delimited-lines
      block-comment-open-close-pair inline-text))
  (gen/sample any-line-comment))

(def block-comment-open-close-pair
  (gen/elements
    [["/*" "*/"]
     #_["(*" "*)"] ;; not for klj
     [";(" ");"]
     [";[" "];"]
     [";{" "};"]
     [";<" ">;"]
     [";\"" "\";"]
     [";'" "';"]
     [";|" "|;"]
     [";!" "!;"]
     [";*" "*;"]
     [";#" "#;"]
     ["#|" "|#"]
     #_["--[[" "]]"]]))

(defn replace-in-line [{:keys [line eol]} pat rep]
  {:line (str/replace line pat rep) :eol eol})

(def block-comment
  (let [span-g (text-span opt-spaces-or-tabs opt-spaces-or-tabs inline-text-non-empty)
        body-g (lines 2 5 span-g eol)]
    (gen/fmap
      (fn [{:keys [body close] :as bc}]
        (let [body (mapv #(replace-in-line % close "__") body)]
          (assoc bc :body body)))
      (delimited-lines
        block-comment-open-close-pair
        body-g))))

(defn block-comment-str [{:keys [open body close]}]
  (let [lines (map #(str (:line %) (:eol %)) body)]
    (str open (str/join lines) close)))

(comment
  (block-comment-str (gen/generate block-comment))
  :eoc)


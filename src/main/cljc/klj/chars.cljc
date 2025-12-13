(ns klj.chars
  (:import java.lang.Character))

(def eol-ch? #{\return \newline})

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (and (not (eol-ch? ch)) (Character/isWhitespace ^Character ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (when ch
    (Character/isDigit ch)))

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (identical? \newline c))

(defn newline-or-nil? [c]
  (or (nil? c) (newline? c)))

(defn delimiter? [ch]
  (case ch
    \( :open
    \) :close
    \[ :open
    \] :close
    \{ :open
    \} :close
    nil))

(def separator? #{\, \; \:})

(def dashes-u
  {:emdash \u2014
   :endash \u2013})

(def dashes
  {:emdash \—
   :endash \–})

(def quote-chars-u
  {:angle-right-pointing \u00BB
   :angle-left-pointing \u00AB
   :double-quote \"
   :double-left \u201C
   :double-right \u201D
   :lo-quote-left \u201E
   :hi-quote-right \u201C
   :single-quote \'
   :single-left \u2018
   :single-right \u2019})

(def quote-chars
  {:single-left \‘,
   :single-right \’,
   :double-right \”,
   :double-quote \",
   :hi-quote-right \“,
   :lo-quote-left \„,
   :single-quote \',
   :double-left \“,
   :angle-left-pointing \«,
   :angle-right-pointing \»})

(def bracket-pairs-u
  {:paren [\( \)]
   :brace [\{ \}]
   :bracket [\[ \]]
   :angle [\u2329 \u232A]
   :curved-angle [\u29FC \u29FD]
   :corner-bracket [\u300C \u300D]
   :dbl-paren [\u2E28 \u2E29]
   :dbl-angle [\u27EA \u27EB]
   :dbl-angle-cjk [\u300A \u300B]
   :floor [\u230A \u230B]
   :ceiling [\u2308 \u2309]
   :top-bracket [\u2E22 \u2E23]
   :bot-bracket [\u2E24 \u2E25]
   :sideways-bracket [\u2E26 \u2E27]
   :wiggly [\u29D8 \u29D9]
   :dbl-wiggly [\u29DA \u29DB]})

(def bracket-pairs
  {:dbl-angle [\⟪ \⟫],
   :dbl-paren [\⸨ \⸩],
   :brace [\{ \}],
   :ceiling [\⌈ \⌉],
   :angle [\〈 \〉],
   :paren [\( \)],
   :bot-bracket [\⸤ \⸥],
   :bracket [\[ \]],
   :top-bracket [\⸢ \⸣],
   :dbl-angle-cjk [\《 \》],
   :corner-bracket [\「 \」],
   :curved-angle [\⧼ \⧽],
   :sideways-bracket [\⸦ \⸧],
   :floor [\⌊ \⌋],
   :wiggly [\⧘ \⧙],
   :dbl-wiggly [\⧚ \⧛]})

(def matching-bracket
  (->> bracket-pairs vals flatten (apply hash-map)))


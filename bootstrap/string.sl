#lang s-exp "sl.rkt"


; TODO this style of "import" is bad because it makes it look like
; string.sl is defining all these operations.
; or maybe I just need to be more explicit about exports?
(def <= int.<=)

(def and2 bool.and2)
(def or2 bool.or2)
(def not bool.not)

(def empty list.empty)
(def cons list.cons)

; The two essential string primitives are explode and implode.
; These let you reuse nats and lists to specify strings.

(def (string-append* strings)
  (match strings
    [(empty) ""]
    [(cons s ss) (string-append s (string-append* ss))]))

(def (string-chars s)
  (match s
    ["" (empty)]
    [s (cons (substring s 0 1)
             (string-chars (substring* s 1)))]))

(def (substring* s start)
  (substring s start (string-length s)))


(def (startswith? s prefix)
  (match (<= (string-length prefix)
             (string-length s))
    ; the string must be at least as long as the prefix
    [#false #false]
    [#true
     (string=? prefix
               (substring s 0 (string-length prefix)))]))


(def (string-split s sep)
  (match (startswith? s sep)
    [#true (cons "" (string-split (substring* s (string-length sep))
                                  sep))]
    [#false (match s
              ; This is Python's take on string-split--
              ; another option is to say (string-split "" _) == (empty),
              ; but that seems to imply  (string-split "x," ",") == (list "x")
              ; when (list "x" "") would make more sense.
              ["" (list "")]
              [s (match (string-split (substring* s 1) sep)
                   [(cons x xs) (cons (string-append (substring s 0 1)
                                                     x)
                                      xs)])])]))


(def (char-in-range? c start end) ; inclusive
  (match (ord c)
    [c
     (match (ord start)
       [start
        (match (ord end)
          [end
           (and2 (<= start c)
                 (<= c end))])])]))


; ascii table: https://www.unicode.org/charts/PDF/U0000.pdf

(def (ascii? c)
  ; ascii characters are 7 bits.
  ; 128 is 1000 0000, the smallest number with its 8th bit set.
  (< (ord c) 128))

(def (digit? c)
  (char-in-range? c "0" "9"))

(def (uppercase? c)
  (char-in-range? c "A" "Z"))

(def (lowercase? c)
  (char-in-range? c "a" "z"))

(def (alpha? c)
  (or2 (uppercase? c)
       (lowercase? c)))

(def (alphanumeric? c)
  (or2 (digit? c)
       (alpha? c)))

(def (whitespace? c)
  ; http://www.cplusplus.com/reference/cctype/isspace/
  (or2 (= (ord c) (ord " "))
       (char-in-range? c "\t" "\r")))

(def (printable? c)
  (char-in-range? c " " "~"))

(def (graphical? c)
  (and2 (printable? c)
        (not (= (ord c) (ord " ")))))

(def (punctuation? c)
  (and2 (graphical? c)
        (not (alphanumeric? c))))

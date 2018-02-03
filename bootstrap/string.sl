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

; re-export this prim
(def (append s1 s2)
  (string-append s1 s2))

(def (append* strings)
  (match strings
    [(empty) ""]
    [(cons s ss) (string-append s (append* ss))]))

(def (chars s)
  (match s
    ["" (empty)]
    [s (cons (substring s 0 1)
             (chars (slice* s 1)))]))

; re-export this prim
(def (slice s start end)
  (substring s start end))

(def (slice* s start)
  (slice s start (string-length s)))

(def (startswith? s prefix)
  ; TODO use and
  ; the string must be at least as long as the prefix
  (if (<= (string-length prefix)
          (string-length s))
      (string=? prefix
                (substring s 0 (string-length prefix)))
      #false))

(def (split s sep)
  (if (startswith? s sep)
      (cons "" (split (slice* s (string-length sep))
                      sep))
      (match s
        ; This is Python's take on split--
        ; another option is to say (split "" _) == (empty),
        ; but that seems to imply  (split "x," ",") == (list "x")
        ; when (list "x" "") would make more sense.
        ["" (list "")]
        [s (match (split (slice* s 1) sep)
             [(cons x xs) (cons (string-append (substring s 0 1)
                                               x)
                                xs)])])))


; ascii table: https://www.unicode.org/charts/PDF/U0000.pdf

; TODO move these to a "char" module? reads nicer: char.digit? vs string.digit?
(def (char-in-range? c start end) ; inclusive
  (match (ord c)
    [c
     (match (ord start)
       [start
        (match (ord end)
          [end
           (and2 (<= start c)
                 (<= c end))])])]))

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

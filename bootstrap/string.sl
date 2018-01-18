

; The two essential string primitives are explode and implode.
; These let you reuse nats and lists to specify strings.

(def (string-length str)
  (length (explode str)))

(def (substring s start end)
  (implode (take (- end start)
                 (drop start
                       (explode s)))))

(def (ord s)
  (match (explode s)
    [(cons i (empty)) i]))

(def (chr i)
  (implode (cons i (empty))))

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

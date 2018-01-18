

; The two essential string primitives are explode and implode.
; These let you reuse nats and lists to specify strings.

(def (string-length str)
  (length (explode str)))

(def (substring start end))

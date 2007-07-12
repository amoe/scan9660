; FIXME.  Would be better to treat records as vectors since we are randomly
; accessing them.

(module directory
  (start
   length
   identifier-length
   identifier)

  (import data)

  (define (start rec)
    (both-endian-double-word->integer (list-slice rec 2 8)))

  (define (length rec)
    (both-endian-double-word->integer (list-slice rec 10 8)))

  (define (identifier-length rec)
    (list-ref rec 32))

  (define (identifier rec)
    (list->string
      (map integer->char (list-slice rec 33 (identifier-length rec))))))

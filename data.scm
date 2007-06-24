(module data
  (both-endian-double-word?)

  (import srfi-1)
  (import srfi-8)

  (define (both-endian-double-word? lst)
    (and
      (= (length lst) 8)
      (receive (le be) (split-at lst 4)
        (list= = le (reverse be))))))

(module lib
  (say
   inc
   dec
   in-range?)

  (define (say msg)
    (display msg)
    (newline))

  (define (inc x) (+ x 1))
  (define (dec x) (- x 1))

  (define (in-range? x start end)
    (and (>= x start) (<= x end))))

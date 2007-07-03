(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-8)
(require-library 'sisc/libs/srfi/srfi-26)

(import lib)

(define *version* 1)

(define main (lambda args #t))

(define (version)
  (say (format "This is scan9660 version ~a." *version*))
  (say "Written by David Banks <amoebae@gmail.com>.")
  (say "This program is in the public domain."))


(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-8)
(require-library 'sisc/libs/srfi/srfi-9)
(require-library 'sisc/libs/srfi/srfi-26)

(load "lib.scm")
(load "data.scm")
(load "heuristic.scm")
(load "io.scm")
(load "scan9660.scm")

(import lib)

(define *version* 1)

(define (test)
  (import scan9660)
  (scan9660 "/home/amoe/dev/scheme/scan9660/test.iso"))

(define main (lambda args #t))

(define (version)
  (say (format "This is scan9660 version ~a." *version*))
  (say "Written by David Banks <amoebae@gmail.com>.")
  (say "This program is in the public domain."))


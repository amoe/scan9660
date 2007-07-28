; FIXME.  Would be better to treat records as vectors since we are randomly
; accessing them.
; Please note that this does NOT represent a directory - just a
; directory RECORD (under 9660 terminology).
; It's more likely to represent a file.

(module directory
  (directory->job)

  (import srfi-13)
  (import srfi-19)

  (import io)
  (import data)
  (import munger)
  (import schedule)

  (define (directory->job rec)
    (let ((s (start rec))  (l (length rec)))
      (make-job
        (munge (identifier rec))
        (date->time-monotonic (date rec))
        (type rec)
        s
        (+ s (quotient l sector-size))
        (remainder l sector-size))))

  (define (start rec)
    (both-endian-double-word->integer (list-slice rec 2 8)))

  (define (length rec)
    (both-endian-double-word->integer (list-slice rec 10 8)))

  ; FIXME: should we change this to a simple boolean?
  (define (type rec)
    (list-ref rec 25))

  (define (identifier-length rec)
    (list-ref rec 32))

  (define (date rec)
    (make-date
      0  ;nanosecond
      (list-ref rec 23)  ;second
      (list-ref rec 22)  ;minute
      (list-ref rec 21)  ;hour
      (list-ref rec 20)  ;day
      (list-ref rec 19)  ;month
      (+ (list-ref rec 18) 1900)  ;year
      0))  ; zone-offset FIXME

  ; can use FILE-SET-LAST-MODIFIED! from file-manipulation to restore mtimes

  (define (identifier rec)
    (list->string
      (map integer->char (list-slice rec 33 (identifier-length rec))))))

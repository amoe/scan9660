; scan9660 - scan an iso9660 filesystem looking for directory sectors
; see what is salvageable
; usage:
; scan9660 FILE...
; where FILE is an iso9660 filesystem
(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-8)

(import srfi-1)
(import srfi-8)
(import binary-io)
(import buffers)

(define *heuristics*
  (list
    (lambda (x) (even? (buffer-ref x 0)))
    (lambda (x) (zero? (buffer-ref x 1)))
    (lambda (x) (both-endian-double-word? (buffer-slice x 2 8)))
    (lambda (x) (both-endian-double-word? (buffer-slice x 10 8)))))

(define *sector-size* 2048)
(define *buffer* (make-buffer *sector-size*))

(define main
  (lambda args
    (for-each scan9660 args)))

(define (scan9660 path)
  (define (read-loop)
    (let ((n (read-block *buffer* 0 *sector-size*)))
      (cond
        ((eof-object? n)
          '())
        (else
          (cons (buffer-slice *buffer* 0 n)
            (read-loop))))))

  (with-binary-input-from-file path read-loop))

(define (process-sector buf) #f)

(define (buffer-slice buf offset length)
  (if (zero? length)
    '()
    (cons
      (buffer-ref buf offset)
      (buffer-slice buf (+ offset 1) (- length 1)))))

(define (zero-slice? lst)
  (every (lambda (x) (= x 0)) lst))

(define (both-endian-double-word? lst)
  (and
    (= (length lst) 8)
    (receive (le be) (split-at lst 4)
      (list= = le (reverse be)))))

(define (say msg)
  (display msg)
  (newline))



(require-extension (srfi 1))
(require-extension (srfi 8))
(require-extension (srfi 9))
(require-extension (srfi 26))
(require-extension (srfi 28))

(import binary-io)
(import buffers)

(define-record-type <sector>
  (make-sector buffer length number)
  sector?
  (buffer :buffer :set-buffer!)    ; buffer containing sector data
  (length :length :set-length!)    ; length of buffer
  (number :number :set-number!))   ; zero-based sector number

(define *test-file* "/home/amoe/dev/scheme/scan9660/test.iso")
(define *sector-size* 2048)
(define *buffer* (make-buffer *sector-size*))

(define main
  (lambda args
    (for-each scan9660 args)))

(define (scan9660 path)
  (call-with-binary-input-file path
    (cut for-each-buffer process-sector <>)))

(define (for-each-buffer proc port)
  (define (loop address)
    (let ((n (read-block *buffer* 0 *sector-size* port)))
      (cond
        ((eof-object? n)
          #t)
        (else
          (proc (make-sector *buffer* n address))
          (loop (inc address))))))

  (loop 0))

(define (process-sector sector)
  (let ((p (apply-heuristics sector)))
    (when (= p 1)
      (say
        (format "~a (~a): ~a  -> ~a"
          (:number sector)
          (:length sector)
          p
          (info (:buffer sector)))))))

(define (info buffer)
  (format "data address: ~a, length: ~a"
    (buffer-slice buffer 2 8)
    (buffer-slice buffer 10 8)))

(define *heuristics*
  (list
    (lambda (buf)
      "First byte is nonzero and even"
      (let ((b0 (buffer-ref buf 0)))
        (and (not (zero? b0)) (even? b0))))
    ; We can't use this one because it will bias nonzero buffers
;;     (lambda (buf)
;;       "Second byte is length of extended attribute record, never used in
;;        practice, so it will always be zero."
;;       (zero? (buffer-ref buf 0)))))
    ; Next some BEDWs
    (lambda (buf) (both-endian-double-word? (buffer-slice buf 2 8)))
    (lambda (buf) (both-endian-double-word? (buffer-slice buf 10 8)))

    ; Date heuristics
    (lambda (buf) (in-range? (buffer-ref buf 19) 1 12))
    (lambda (buf) (in-range? (buffer-ref buf 20) 1 31))
    (lambda (buf) (in-range? (buffer-ref buf 21) 0 23))
    (lambda (buf) (in-range? (buffer-ref buf 22) 0 59))
    (lambda (buf) (in-range? (buffer-ref buf 23) 0 59))
    
    ; Normal file or directory
    (lambda (buf)
      (let ((flag (buffer-ref buf 25)))
        (or (zero? flag) (= flag 2))))

    ; Interleaving (all zeroes)
    (lambda (buf) (zero? (buffer-ref buf 26)))
    (lambda (buf) (zero? (buffer-ref buf 27)))

    ; both endian one: 01 00 00 01
    (lambda (buf) (= (buffer-ref buf 28) 1))
    (lambda (buf) (zero? (buffer-ref buf 29)))
    (lambda (buf) (zero? (buffer-ref buf 30)))
    (lambda (buf) (= (buffer-ref buf 31) 1))))

; Inclusive range test
(define (in-range? x start end)
  (and (>= x start) (<= x end)))

(define (both-endian-double-word? lst)
  (and
    (= (length lst) 8)
    (receive (le be) (split-at lst 4)
      (list= = le (reverse be)))))

; Apply heuristics one by one to sector
; Return probability
(define (apply-heuristics sector)
  (/
    (count identity
      (map (cut <> (:buffer sector)) *heuristics*))
    (length *heuristics*)))

(define (directory-record? buf)
  (let ((b0 (buffer-ref buf 0)))
    (and (not (zero? b0)) (even? b0))))

(define (classify buffer) #t)

(define (split-directory-records buffer) #t)

(define (buffer-slice buf offset length)
  (if (zero? length)
    '()
    (cons
      (buffer-ref buf offset)
      (buffer-slice buf (+ offset 1) (- length 1)))))

(define (say msg)
  (display msg)
  (newline))

(define (inc x) (+ x 1))

(define (identity x) x)

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
    (cut for-each-sector process-sector <>)))

(define (for-each-sector proc port)
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
      (print-indented 0 (format "Sector: ~a" (:number sector)))
      (print-indented 0 (format "Length: ~a" (:length sector)))
      (print-indented 0 (format "Heuristic response: ~a" p))

      (for-each process-directory-record
        (split-directory-records (:buffer sector))))))

(define (process-directory-record record)
  (print-indented 1 (format "Identifier: ~s" (extract-identifier record)))
  (print-indented 1
    (format "Starting sector: ~a" (extract-start record)))
  (print-indented 1
    (format "Length in bytes: ~a" (extract-length record))))
  
(define (info buffer)
  (let ((records (split-directory-records buffer)))
    (format "identifiers: ~s"
      (map extract-identifier records))))

(define (split-directory-records buf)
  (define (loop start)
    (let ((len (buffer-ref buf start)))
      (if (zero? len)
        '()
        (cons
          (buffer-slice buf start len)
          (loop (+ start len))))))

  (loop 0))

(define (extract-start directory-record)
  (both-endian-double-word->integer (list-slice directory-record 2 8)))

(define (extract-length directory-record)
  (both-endian-double-word->integer (list-slice directory-record 10 8)))

(define (print-indented level msg)
  (define (generate-indent level)
    (if (zero? level) "" (string-append "  " (generate-indent (dec level)))))

  (say (format "~a~a" (generate-indent level) msg)))


(define (extract-identifier directory-record)
  (let ((len (list-ref directory-record 32)))
    (list->string
      (map integer->char (list-slice directory-record 33 len)))))

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
    (count (cut <> (:buffer sector)) *heuristics*)
    (length *heuristics*)))

(define (directory-record? buf)
  (let ((b0 (buffer-ref buf 0)))
    (and (not (zero? b0)) (even? b0))))

(define (classify buffer) #t)

(define (buffer-slice buf offset length)
  (if (zero? length)
    '()
    (cons
      (buffer-ref buf offset)
      (buffer-slice buf (inc offset) (dec length)))))

(define (say msg)
  (display msg)
  (newline))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

; Integer conversions, thanks riastradh

(define (both-endian-double-word->integer bedw)
  ; Use the big endian representation only
  (big-endian-double-word->integer (take-right bedw 4)))

(define (big-endian-double-word->integer bedw)
  (define (loop bedw factor)
    (if (null? (cdr bedw))
      (car bedw)
      (+ (arithmetic-shift (car bedw) factor)
         (loop (cdr bedw) (/ factor 2)))))

  (loop bedw (* (length bedw) 8)))

(define (arithmetic-shift integer exponent)
  (* integer (expt 2 exponent)))

(define (list-slice lst start length)
  (take (drop lst start) length))

;; < amoe> can anyone give me a search term for the algorithm to make integers from octets i mentioned above?
;; < Riastradh> sarahbot: eval (let ((a #xDE) (b #xAD)) (number->string (+ (* a (expt 2 16)) b) 16))
;; < sarahbot> "de00ad"
;; < Riastradh> sarahbot: eval (let ((a #xDE) (b #xAD)) (number->string (+ (* a (expt 2 4)) b) 16))
;; < sarahbot> "e8d"
;; < amoe> aha, i think i see
;; < Riastradh> amoe, ignoring the second one (where I meant `8', not `4'), does that give you an idea?
;; < amoe> yes, it makes sense
;; < amoe> thanks riastradh
;; < Riastradh> amoe, now, (ARITHMETIC-SHIFT integer exponent) = (* integer (EXPT 2 exponent)). BITWISE-IOR is the bitwise inclusive-or, which would work just as well as + if the two integers are disjoint.
;; *pjd* nudges amoe in the direction of fold
;; *amoe* scurries away

; Basic pattern of data usage is as such:
; Initially, buffers used for speedy heuristic application.
; If a sector containing consecutive directory records is identified, split it 
; to a list of directory records.

(module data
  (both-endian-double-word?
   sector->directory-records
   buffer-slice

   make-sector  sector?
   :buffer :set-buffer!  :length :set-length!  :number :set-number!)

  (import srfi-1)
  (import srfi-8)
  (import srfi-9)

  (import lib)

  ; We can't just pass buffers around and use buffer-length, because
  ; not enough data might have been read.  We could use buffer-copy! and
  ; make a new buffer of exactly the right size, but this seems somewhat
  ; wasteful.  In addition, we can carry the sector number around with us
  ; for debugging purposes.
  (define-record-type <sector>
    (make-sector buffer length number)
    sector?
    (buffer :buffer :set-buffer!)
    (length :length :set-length!)
    (number :number :set-number!))

  ; FIXME: add index out of bound checks
  (define (sector->directory-records sector)
    (define buf (:buffer sector))
    
    (define (loop start)
      (let ((len (buffer-ref buf start)))
        (if (zero? len)
          '()
          (cons
            (buffer-slice buf start len)
            (loop (+ start len))))))

    (loop 0))

  (define (buffer-slice buf offset length)
    (if (zero? length)
      '()
      (cons
        (buffer-ref buf offset)
        (buffer-slice buf (inc offset) (dec length)))))

  (define (both-endian-double-word? lst)
    (and
      (= (length lst) 8)
      (receive (le be) (split-at lst 4)
        (list= = le (reverse be))))))

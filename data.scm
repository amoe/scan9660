; Basic pattern of data usage is as such:
; Initially, buffers used for speedy heuristic application.
; If a sector containing consecutive directory records is identified, split it 
; to a list of directory records.

(module data
  (both-endian-double-word?

   make-sector  sector?
   :buffer :set-buffer!  :length :set-length!  :number :set-number!)

  (import srfi-1)
  (import srfi-8)
  (import srfi-9)

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

  (define (both-endian-double-word? lst)
    (and
      (= (length lst) 8)
      (receive (le be) (split-at lst 4)
        (list= = le (reverse be))))))

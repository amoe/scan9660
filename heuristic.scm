(module heuristic
  (apply-heuristics)

  (import srfi-1)
  (import srfi-26)
  (import buffers)

  (import lib)
  (import data)
  
  (define table '())

  (define (add heuristic)
    (set! table (cons heuristic table)))

  (define (finalize)
    (set! table (reverse table)))

  (define (apply-heuristics sector)
    (/
      (count (cute <> sector) table)
      (length table)))

  (define (byte-heuristic address pred?)
    (lambda (sector)
      (if (< (dec (:length sector)) address)
        (error "not enough buffered data to apply heuristic: byte ~a needed"
          address)
        (pred? (buffer-ref (:buffer sector) address)))))
  
  ; record length
  (add (byte-heuristic 0 even?))
  ; extended-attribute-record-length 
  (add (byte-heuristic 1 zero?))

  ; month
  (add (byte-heuristic 19 (cute in-range? <> 1 12)))
  ; day
  (add (byte-heuristic 20 (cute in-range? <> 1 31)))
  ; hour
  (add (byte-heuristic 21 (cute in-range? <> 0 23)))
  ; minute
  (add (byte-heuristic 22 (cute in-range? <> 0 59)))
  ; second
  (add (byte-heuristic 23 (cute in-range? <> 0 59)))

  ; flags: normal file or directory
  (add 
    (byte-heuristic 25
      (lambda (x)
        (or (zero? x) (= x 2)))))
   ;interleave-unit-size 
  (add (byte-heuristic 26 zero?))
  ; interleave-gap-size  
  (add (byte-heuristic 27 zero?))

  ; volume-sequence: both endian word
  (add (byte-heuristic 28 (cute = <> 1)))
  (add (byte-heuristic 29 zero?))
  (add (byte-heuristic 30 zero?))
  (add (byte-heuristic 31 (cute = <> 1)))

  (finalize))

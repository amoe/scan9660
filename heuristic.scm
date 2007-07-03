(module heuristic
  ; Record length
  (define record-length (byte-heuristic 0 even?))

  (define extended-attribute-record-length (byte-heuristic 1 zero?))

  (define month  (byte-heuristic 19 (cute in-range? <> 1 12)))
  (define day    (byte-heuristic 20 (cute in-range? <> 1 31)))
  (define hour   (byte-heuristic 21 (cute in-range? <> 0 23)))
  (define minute (byte-heuristic 22 (cute in-range? <> 0 59)))
  (define second (byte-heuristic 23 (cute in-range? <> 0 59)))

  ; normal file or directory
  (define flags
    (byte-heuristic 25
      (lambda (x)
        (or (zero? x) (= x 2)))))

  (define interleave-unit-size (byte-heuristic 26 zero?))
  (define interleave-gap-size  (byte-heuristic 27 zero?))

  (define volume-sequence-0 (byte-heuristic 28 (cute = <> 1)))
  (define volume-sequence-1 (byte-heuristic 29 zero?))
  (define volume-sequence-2 (byte-heuristic 30 zero?))
  (define volume-sequence-4 (byte-heuristic 31 (cute = <> 1))))

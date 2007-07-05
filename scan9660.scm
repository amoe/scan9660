(module scan9660
  (scan9660)

  (import srfi-26)

  (import lib)
  (import io)
  (import heuristic)

  (define (scan9660 path)
    (call-with-binary-input-file path
      (cut for-each-sector scan-sector <>)))

  (define (scan-sector sector)
    (let ((p (apply-heuristics sector)))
      (if (= p 1)
        (say "Found sector probably containing directory records")
        (say "Skipping")))))

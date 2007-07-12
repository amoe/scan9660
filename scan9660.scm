(module scan9660
  (scan9660)

  (import srfi-26)

  (import lib)
  (import io)
  (import data)
  (import heuristic)

  (define (scan9660 path)
    (call-with-binary-input-file path
      (cut for-each-sector scan-sector <>)))

  (define (scan-sector sector)
    (let ((p (apply-heuristics sector)))
      (if (= p 1)
        (begin
          (say "Found sector probably containing directory records")
          (for-each scan-directory-record (sector->directory-records sector)))
        (say "Skipping sector"))))

  (define (scan-directory-record rec)
    (import directory)

    (say (format "Starting sector: ~a" (start rec)))
    (say (format "Length in bytes: ~a" (length rec)))
    (say (format "Identifier: ~a" (identifier rec)))))

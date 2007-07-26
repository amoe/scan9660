(module scan9660
  (scan9660
   file-table)

  (import srfi-19)
  (import srfi-26)

  (import lib)
  (import io)
  (import data)
  (import heuristic)

  (define file-table '())

  (define (scan9660 path)
    (call-with-binary-input-file path
      (cut for-each-sector scan-sector <>)))

  (define (scan-sector sector)
    (if (memv (:number sector) file-table)
      (salvage-sector sector)
      (heuristic-scan sector)))

  (define (heuristic-scan sector)
    (let ((p (apply-heuristics sector)))
      (if (= p 1)
        (begin
          (say "Found sector probably containing directory records")
          (for-each scan-directory-record (sector->directory-records sector)))
        ;(say "Skipping sector"))))
        #f)))

  (define (salvage-sector sector)
    (cond
      ((null-sector? sector)
        (say (format "Not salvaging null sector ~a" (:number sector))))

      (else
        (say (format "Salvaging sector: ~a" (:number sector)))

        (call-with-binary-output-file (format "sector/~a.data" (:number sector))
          (lambda (port)
            (write-block (:buffer sector) 0 (:length sector) port))))))

  (define (scan-directory-record rec)
    (import directory)
    (import file-info)

    (set! file-table (cons (:start (directory->file-info rec)) file-table))))

    ;(display (pretty-print (directory->file-info rec)))))

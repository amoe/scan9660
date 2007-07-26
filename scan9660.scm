(module scan9660
  (scan9660
   file-table)

  (import srfi-1)
  (import srfi-19)
  (import srfi-26)

  (import lib)
  (import io)
  (import data)
  (import heuristic)
  (import file-info)

  (define file-table '())

  (define (scan9660 path)
    (call-with-binary-input-file path
      (cut for-each-sector scan-sector <>)))

  (define (table-searcher n)
    (lambda (f)
      (eqv? n (:start f))))

  (define (scan-sector sector)
    (let ((f (find (table-searcher (:number sector)) file-table)))
      (if f
        (salvage-sector sector f)
        (heuristic-scan sector))))

  (define (heuristic-scan sector)
    (let ((p (apply-heuristics sector)))
      (if (= p 1)
        (begin
          (say "Found sector probably containing directory records")
          (for-each scan-directory-record (sector->directory-records sector)))
        ;(say "Skipping sector"))))
        #f)))

  (define (salvage-sector sector f)
    (cond
      ((null-sector? sector)
        (say
          (format "Not salvaging null sector ~a (~a)"
            (:number sector)
            (:identifier f))))

      (else
        (say
          (format "Salvaging sector: ~a (~a)"
            (:number sector)
            (:identifier f)))

        (call-with-binary-output-file (format "sector/~a.data" (:number sector))
          (lambda (port)
            (write-block (:buffer sector) 0 (:length sector) port))))))

  (define (scan-directory-record rec)
    (import directory)
    (import file-info)

    (set! file-table (cons (directory->file-info rec) file-table))))

    ;(display (pretty-print (directory->file-info rec)))))

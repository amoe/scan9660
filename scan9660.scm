(module scan9660
  (scan9660)

  (import srfi-1)
  (import srfi-19)
  (import srfi-26)

  (import lib)
  (import io)
  (import data)
  (import heuristic)
  (import directory)
  (import schedule)

  (define (scan9660 path)
    (call-with-binary-input-file path
      (cut for-each-sector scan-sector <>)))

  (define (scan-sector sector)
    (let ((job (scheduled (:number sector))))
      (if job
        (salvage-sector sector job)
        (heuristic-scan sector))))

  (define (heuristic-scan sector)
    (let ((p (apply-heuristics sector)))
      (if (= p 1)
        (begin
          (say "Found sector probably containing directory records")
          (for-each scan-directory-record (sector->directory-records sector)))
        #f)))

  (define (salvage-sector sector job)
    (define (msg s j)
      (format "~a (~a)" (:number s) (:path j)))
    
    (cond
      ((null-sector? sector)
        (say (format "Not salvaging null sector: ~a" (msg sector job))))

      (else
        (say (format "Salvaging sector: ~a" (msg sector job)))

        (call-with-binary-output-file (format "sector/~a.data" (:number sector))
          (lambda (port)
            (write-block (:buffer sector) 0 (:length sector) port))))))

  (define (scan-directory-record rec)
    (schedule! (directory->job rec))))

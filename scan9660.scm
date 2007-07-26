(module scan9660
  (scan9660)

  (import srfi-1)
  (import srfi-19)
  (import srfi-26)

  (import lib)
  (import io)
  (import data)
  (import heuristic)

  (define schedule '())

  (define (scan9660 path)
    (call-with-binary-input-file path
      (cut for-each-sector scan-sector <>)))

  (define (scheduled? n)
    (import file-info)

    (lambda (f)
      (let* ((start (:start f))
             (end (+ start (/ (:length f) sector-size))))
        (and (>= n start) (< n end)))))

  (define (scan-sector sector)
    (let ((f (find (scheduled? (:number sector)) schedule)))
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
    (define (msg s f)
      (import file-info)
      (format "~a (~a)" (:number s) (:identifier f)))
    
    (cond
      ((null-sector? sector)
        (say (format "Not salvaging null sector: ~a" (msg sector f))))

      (else
        (say (format "Salvaging sector: ~a" (msg sector f)))

        (call-with-binary-output-file (format "sector/~a.data" (:number sector))
          (lambda (port)
            (write-block (:buffer sector) 0 (:length sector) port))))))

  (define (scan-directory-record rec)
    (import directory)
    (import file-info)

    (set! schedule (cons (directory->file-info rec) schedule))))

    ;(display (pretty-print (directory->file-info rec)))))

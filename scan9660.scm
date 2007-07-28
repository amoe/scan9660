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

  ; THE PROBLEM: Directory records refer to their own sectors.
  ; THE CAUSE: Directory records for '.', '..', and other things.
  ; THE SOLUTION: Ignore all jobs scheduled for directories for the moment.

  (define (scan-sector sector)
    ;(say (format "Scanning sector: ~a"  (:number sector)))
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

  (define (maybe-open-file n job)
    (when (= n (:first job))
      (say "Opening file")
      ; It's buffered, so remember to flush it
      (:set-port! job
        (open-buffered-binary-output-port
          (open-binary-output-file
            (format "sector/~a" (:path job)))))))

  (define (write-sector sector job)
    (let ((n (:number sector)) (buf (:buffer sector))  (port (:port job)))
      (cond
        ((= n (:last job))    ; the final sector
          (say
            (format "Sector ~a, last sector ~a, first ~a, tail ~a"
              n (:last job) (:first job) (:tail job)))
          ; For some reason writing this produces nothing in the files.
          ; It works if you write blocks using call-with-binary-output-file
          ; instead.  Suggesting it may be a cleanup issue; the output is not
          ; getting flushed.
          (write-block buf 0 (:tail job) port)
          (flush-output-port port)
          (:set-port! job #f))  ; FIXME: does this get GCed and closed?
        (else
          (write-block buf 0 (:length sector) port)))))

    (define (salvage-sector sector job)
      (define (msg s j)
        (format "~a (~a)" (:number s) (:path j)))

      (say "Maybe opening file")
      (maybe-open-file (:number sector) job)
      
      (cond
        ((null-sector? sector)
          (say (format "Not salvaging null sector: ~a" (msg sector job))))
      (else
        (say (format "Salvaging sector: ~a" (msg sector job)))
        (write-sector sector job))))

  (define (scan-directory-record rec)
    (schedule! (directory->job rec))))

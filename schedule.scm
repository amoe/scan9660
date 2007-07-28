(module schedule
  (make-job
   :first
   :last
   :path
   :port
   :set-port!
   :tail

   scheduled
   schedule!)

  (import srfi-1)
  (import srfi-9)

  (define jobs '())

  ; takes sector number, maps to matching job using FIND
  (define (scheduled sn)
    (find
      (lambda (j) (and (>= sn (:first j)) (<= sn (:last j))))
      jobs))

  ; throw away directory jobs for the moment
  (define (schedule! job)
    (when (zero? (:type job)) ; as long as it is a regular file
      (set! jobs (cons job jobs))))

  ; Further processed info.
  ; port is an open or closed binary output stream.
  ; mtime is seconds since epoch.
  ; path is a case-converted filename with unicode removed.
  ; first is the first sector to save.
  ; last is the last sector.
  ; tail is the number of bytes to save from the last sector.
  ; TAIL can be calculated from start and length thus:
  ; (remainder length sector-size)
  ; LAST is
  ; (+ start (quotient length sector-size))
  (define-record-type <job>
    (make-job path mtime type first last tail)
    job?
    (port  :port  :set-port!)
    (path  :path  :set-path!)
    (mtime :mtime :set-mtime!)
    (type  :type  :set-type!)
    (first :first :set-first!)
    (last  :last  :set-last!)
    (tail  :tail  :set-tail!)))

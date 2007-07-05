(module io
  (sector-size
   for-each-sector)

  (import binary-io)

  (import lib)
  (import data)

  (define sector-size 2048)

  (define (for-each-sector proc port)
    (define (loop number)
      (define buf (make-buffer sector-size))

      (let ((length (read-block buf 0 sector-size port)))
        (cond
          ((eof-object? length)
            #t)
          (else
            (proc (make-sector buf length number))
            (loop (inc number))))))

    (loop 0)))


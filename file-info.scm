; Quick brainstorm:
; We have to read the whole file because any part of it could be a directory
; record.
; -> So we can't use SKIP.
; We'll have to keep a global table of extracted file information and consult
; it on every new sector to see whether the new sector's number corresponds to
; the information we kept.
; We can use the length information to handle the case where 

(module file-info
  (make-file-info
   pretty-print)

  (import srfi-9)
  (import srfi-19)

  ; be careful of clashes with other FORMAT
  (define (pretty-print fi)
    (string-append
      (format "Identifier: ~a~%" (:identifier fi))
      (format "Timestamp: ~a~%"
        (time-second (date->time-monotonic (:timestamp fi))))
      (format "Start: ~a~%" (:start fi))
      (format "Length: ~a~%" (:length fi))
      (format "Type: ~a~%" (:type fi))))

  (define-record-type <file-info>
    (make-file-info identifier timestamp start length type)
    file-info?
    (identifier :identifier :set-identifier!)
    (timestamp  :timestamp  :set-timestamp!)
    (start      :start      :set-start!)
    (length     :length     :set-length!)
    (type       :type       :set-type!)))

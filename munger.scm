; Munge 9660 filenames for use on the local filesystem

(module munger
  (munge)

  (import srfi-1)
  (import srfi-13)

  (define (munge id)
    (let ((s (string-downcase (remove-version id))))
      (if (is-utf16? s) (fix-utf16 s) s)))

  (define (remove-version s)
    (if (string-suffix? ";1" s)
      (string-drop-right s 2)
      s))
  ; For some reason some identifiers in my test filesystem are not strict 9660
  ; identifiers, instead they look like UTF-16 Joliet identifiers.  Christ
  ; knows why.  Anyway, to handle these we need to do some munging.

  ; But since we eventually need to get odd elements as well this must be
  ; generalized.  Oh, actually, odd elements is just (even-elements (cdr lst))!
  (define (even-elements lst)
    (cond
      ((null? lst)  '())
      (else
        (let ((next (cdr lst)))
          (cons (car lst)
                (even-elements
                  (if (null? next) next (cdr next))))))))

;;   (define (odd-elements lst)
;;     (cond
;;       ((null? lst)  '())
;;       (else
;;         (let ((next (cdr lst)))
;;           (if (null? next)
;;             '()
;;             (cons (car next) (odd-elements (cdr next))))))))

  (define (odd-elements lst)
    (even-elements (cdr lst)))
  
  (define (is-utf16? id)
    (every zero? (map char->integer (even-elements (string->list id)))))

  (define (fix-utf16 id)
    (list->string (odd-elements (string->list id)))))

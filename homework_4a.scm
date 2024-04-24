;;1. Мемоизация
(define memoized-factorial
  (let ((known-results '()))
    (lambda (n)
      (let* ((arg (list n))
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (let ((res (if (<= n 1)
                           1
                           (* n (memoized-factorial (- n 1))))))
              (set! known-results (cons (list arg res) known-results))
              res))))))
;;2. Отложенные вычисления
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b)
     (let ((b-promise (delay b)))
       (cons a b-promise)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (if (> k 0)
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))
      '()))

(define (lazy-ref xs k)
  (if (= k 0)
      (car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))

(define (factorials)
  (let loop ((p 1) (n 1))
    (lazy-cons (* p n) (loop (* p n) (+ n 1)))))

(define (lazy-factorial n)
  (car (reverse (lazy-head (factorials) n))))

;;3. Чтение из потока
(define (words-output-from-file way)
  (define (loop way res chek)
    (let ((x (read-char way)))
      (if (eof-object? x)
          res
          (if (and (equal? chek "") (or (equal? x #\space) (equal? x #\newline)))
              (loop way res "")
              (if (equal? x #\space)
                  (loop way (append res (list chek)) "")
                  (loop way res (string-append chek (string x))))))))
  (call-with-input-file way (lambda (port) (write (loop port '() "")))))

;;4. Структуры (записи)
(define __eval__ (lambda (x) (eval x (interaction-environment))))

(define (reversible-reversing-string-to-symbol s)
  (or (and (symbol? s) (symbol->string s)) (string->symbol s)))

(define-syntax define-struct
  (syntax-rules ()
    ((_ name (field-value ...))
     (begin
       (__eval__ (list 'define
                       (reversible-reversing-string-to-symbol
                        (string-append "make-" (reversible-reversing-string-to-symbol (quote name))))
                       (lambda (field-value ...)
                         (list (list  'type-of-struct (quote name)) (list 'field-value field-value) ...))))
       (__eval__ (list 'define
                       (reversible-reversing-string-to-symbol
                        (string-append (reversible-reversing-string-to-symbol (quote name)) "?"))
                       (lambda (x)
                         (and (pair? x)
                              (pair? (car x))
                              (assoc 'type-of-struct x)
                              (not (null? x))
                              (equal? (cadr (assoc 'type-of-struct x)) (quote name))))))
       (__eval__ (list 'define
                       (reversible-reversing-string-to-symbol
                        (string-append
                         (reversible-reversing-string-to-symbol (quote name))
                         "-"
                         (reversible-reversing-string-to-symbol 'field-value)))
                       (lambda (x)
                         (cadr (assoc 'field-value (cdr x))))))
       ...
       (__eval__ (list 'define
                       (reversible-reversing-string-to-symbol
                        (string-append "set-"
                                       (reversible-reversing-string-to-symbol (quote name))
                                       "-"
                                       (reversible-reversing-string-to-symbol 'field-value)
                                       "!"))
                       (lambda (x val)
                         (set-car! (cdr (assoc 'field-value (cdr x))) val)))) ... ))))

;;5. Алгебраические типы данных
(define-syntax define-data
  (syntax-rules ()
    ((_ data-name ((name field-value ...) ...))
     (begin
       (__eval__ (list 'define
                       (quote name)
                       (lambda (field-value ...)
                         (list (list 'name-of-the-algebraic-data-structure (quote data-name))
                               (list 'field-name-of-an-algebraic-data-structure (quote name))
                               (list (quote field-value) field-value)
                               ...))))
       ...
       (__eval__ (list 'define
                       (reversible-reversing-string-to-symbol
                        (string-append
                         (reversible-reversing-string-to-symbol (quote data-name))
                         "?"))
                       (lambda (x)
                         (and (pair? x)
                              (pair? (car x))
                              (>= (length x) 2)
                              (assoc 'name-of-the-algebraic-data-structure x)
                              (equal? (cadr (assoc 'name-of-the-algebraic-data-structure x))
                                      (quote data-name))))))))))

(define-syntax match
  (syntax-rules ()
    ((_ x ((name field-value ...) expr) ...)
     ;(and (pair? x) ((set-cdr! (list field-value ...)) ...)))))

     (cond ((not (pair? x)) x)
           ((equal? (cadadr x) (quote name))
            (let ((field-value (cadr (assoc (quote field-value) x))) ...)
              expr))
           ...
           (else x)))))

;\_______________________________________________λ*\achive/*λ_______________________________________________/
(define-syntax my-let
  (syntax-rules ()
    ((_ ((var val)) . exprs)
     ((lambda (var)  (begin . exprs)) val))
    ((_ ((var val) . xs) . exprs)
     (_  xs ((lambda (var) (begin . exprs)) val)))))
      
(define-syntax my-let*
  (syntax-rules ()
    ((_ ((var val)) . exprs)
     ((lambda (var)  (begin . exprs)) val))
    ((_ ((var val) . xs) . exprs)
     ((lambda (var) (_ xs  . exprs)) val))))

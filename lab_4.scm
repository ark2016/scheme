#|
(define (REPL)
  (let* ((e (read))
         (v (eval e (interaction-environement)))
         (_ (print v)))
    (REPL)))
|#
;;1
(define exit #f)

(define (use-assertions)
  (call-with-current-continuation
   (lambda (now)(set! exit now))))

(define (work arr)
  (let ((dis (car arr))
        (res (car(cdr arr))))
    (cond ((not res) (begin (display "FAILED: ")
                            (write dis)
                            (newline)
                            (exit))))))
(define-syntax assert
  (syntax-rules ()
    ((assert expr)
     (work (list (quote expr) expr)))))
(use-assertions)
#|
(begin (let ((dis (eval expr) (interaction-environment))
             (res (expr)))
         (cond ((not(res)) (begin (display "FAILED: ")
                                  (write dis)))))))))
|#
;;2

(define (save-data str way)
  (with-output-to-file way (lambda () (write str))))
(define (load-data way)
  (call-with-input-file way (lambda (port) (read port)))) 
(define (count-string-in-file way)
  (define (loop way res)
    (let ((x (read way)))
      (if (not (eof-object? x))
          (if (equal? x "")
              (loop way res)
              (loop way (+ res 1)))
          res)))
  (call-with-input-file way
    (lambda (port) (write (loop port 0)))))
;  (call-with-input-file way (lambda (port) (read port))))
;;3
#|
(define func-memo
  (let ((known-results '()))
    (lambda (n)
      (let* ((args (list n))
             (res (assoc args known-results)))
        (if res
            (cadr res)
            (let ((res (t n)))
              (set! known-results (cons (list args res) known-results))
              res))))))
|#
(define t-memo
  (let ((known-results '()))
    (lambda (n)
      (let* ((arg (list n))
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (let ((res (t n)))
              (set! known-results (cons (list arg res) known-results))
              res))))))
      
(define (t n)
  (cond ((<= n 1) 0)
        ((= n 2) 1)
        (else (+ (t (- n 1)) (t (- n 2)) (t (- n 3))))))
;;4
(define-syntax my-if
  (syntax-rules ()
    ((my-if condition one)
     (let ((one-promise (delay one)))
       (or (and condition (force one-promise)) "")))
    ((my-if condition one two)
     (let ((one-promise (delay one))
           (two-promise (delay two)))
       (or (and condition (force one-promise))(and (not condition) (force two-promise)))))))

(define-syntax my-if*
  (syntax-rules ()
    ((my-if condition one)
     (or (and condition one) " "))
    ((my-if condition one two)
     (or (and condition one)(and (not condition) two)))))

;;5
#|
(define-syntax let1
  (syntax-rules ()
    ((let1 ((var val)) expr)
     ((lambda (var) expr) val))))
|#
#|
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))
|#
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) expr ...)
     ((lambda (var ...) expr ...) val ...))))
#|
(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))
|#
(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr ...)
     (let () expr ...)) 
    ((my-let* ((var1 val1) (var2 val2) ...) expr1 expr2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...) expr1 expr2 ...)))))
#|
(define-syntax-rule (let ((id val) ...) body ...)
  ((lambda (id ...) body ...) val ...))
|#
#|
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val)) expr)
     ((lambda (var)expr)val))
    ((my-let ((var val) . (avar aval)) expr)
     ((lambda (var)expr)val))))
(define-syntax res-let
  (syntax-rules (result:)
    ((res-let result: vars vals ((var val)) expr)
     (begin (append vars (list (quote var)))
            (append vals (list val))
            ((lambda(vars)expr)vals)))
    ((res-let result: vars vals ((var val) . (avar aval)) expr)
     (begin (append vars (list var))
            (append vals (list val))
            (res-let result: vars vals (avar aval) expr)))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let ((var val)) expr)
     ((lambda (var)expr)val))
    ((my-let ((var val) . (avar aval)) expr)
     ((lambda (var)expr)val))))
|#
;;6
;;6.1
(define-syntax when
  (syntax-rules ()
    ((when condition one-action) 
     (if (and condition (procedure? one-action ))
         (one-action)))
    ((when condition action . actions)
     (if condition
         (let ((x action))
           (when condition . actions))))))
;
(define-syntax unless
  (syntax-rules ()
    ((unless condition one-action) 
     (if (and (not condition) (procedure? one-action ))
         (one-action)))
    ((unless condition action . actions)
     (if (not condition)
         (let ((x action))
           (when condition . actions))))))
;;6.2
#|
(define-syntax for
  (syntax-rules (:= to downto do)
    ((for var := start to end do . actions)
     (let ((limit end))
       (let loop ((var start))
         (and (<= var limit)
              (begin
                (begin . actions)
                (loop (+ var 1)))))))
    ((for var := start downto end do . actions)
     (let ((limit end))
       (let loop ((var start))
         (and (>= var limit)
              (begin
                (begin . actions)
                (loop (- var 1)))))))))
|#
(define-syntax for
  (syntax-rules (in as)
    ((for var in  xs . expr)
     (if (not (null? xs))
         (let ((var (car xs)))
           (begin
             (begin . epxprs)
             (for var in (quote (cdr xs)) . expr)))))
    ((for xs as var . expr)
     (for var in xs . expr))))
                
;;6.3

(define-syntax while
  (syntax-rules ()
    ((while condition expr)
     (let ((do expr))
       (begin 
         (when condition (while condition expr))
         (write x))))
    ((while condition . expr)
     (if condition
         (begin (begin . expr)
                (while condition . expr))))))
#|
(letrec ((iter (lambda (i)
                 (if (= i 10)
                     '()
                     (cons i (iter (+ i 1)))))))
  (iter 0))
|#
;;6.4
(define-syntax repeat
  (syntax-rules (until)
    ((repeat (x) until condition)
     '())
    ((repeat (x . xs) until condition)
     '())))
     
;;6.5

(define-syntax cout
  (syntax-rules (<<)
    ((cout << x)
     (cond ((equal? (quote x) '<<) (begin (display " ")))
           ((equal? (quote x) 'endl) (begin (newline)))
           (else (display x))))
    ((cout << x . xs)
     (cond ;((equal?  (quote x) '<<) (begin (display " ") (cout << (cdr xs))))
       ((equal?  (quote x) 'endl) (begin (newline) (cout << (cdr xs))))
       (else (begin (display x) (display " ") (cout << xs)))))))

#|
(define-syntax <<
  (syntax-rules (cout)
    ((cout << x)
     (<< x))
    ((<< x)
     (if (equal? x 'endl)
         (newline)
         (display x)))))
|#











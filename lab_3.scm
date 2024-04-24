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
    (cond ((not res) (display "FAILED: ")
                     (write dis)
                     (newline)
                     (exit)))))
(define-syntax assert
  (syntax-rules ()
    ((assert expr)
     (work (list (quote expr) expr)))))
(use-assertions)
;;2

(define (save-data str way)
  (with-output-to-file way (lambda () (write str))))

(define (load-data way)
  (call-with-input-file way (lambda (port) (read port))))

(define (count-string-in-file way)
  (define (loop way res chek)
    (let ((x (read-char way)))
      (if (not (eof-object? x))
          (if (and (equal? chek "") (equal? x #\newline))
              (loop way res "")
              (if (equal? x #\newline)
                  (loop way (+ res 1) x)
                  (loop way res x)))
          res)))
  (call-with-input-file way
    (lambda (port) (write (loop port 0 "")))))
#|
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
|#
;;3
(define t-memo
  (let ((known-results '()))
    (lambda (n)
      (let* ((arg (list n))
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (let ((res (cond ((<= n 1) 0)
                             ((= n 2) 1)
                             (else (+ (t-memo (- n 1)) (t-memo (- n 2)) (t-memo (- n 3)))))))
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
       (force (and condition  one-promise))))
    ((my-if condition one two)
     (let ((one-promise (delay one))
           (two-promise (delay two)))
       (force (or (and condition one-promise) two-promise))))))
#|
(define-syntax my-if*
  (syntax-rules ()
    ((my-if* condition one)
     (or (and condition one) #f))
    ((my-if* condition one two)
     (or (and condition one) two))))
|#
;;5
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) expr ...)
     ((lambda (var ...) expr ...) val ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr ...)
     (my-let () expr ...))
    ((my-let* ((var1 val1) (var2 val2) ...) expr1 ...)
     (my-let ((var1 val1))
             (my-let* ((var2 val2) ...)
                      expr1 ...)))))
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
(define-syntax for
  (syntax-rules (in as)
    ((for i in xs expr ...)
     (begin (define (loop xs*)
              (when (not (null? xs*))
                (let ((i (car xs*)))
                  (begin expr ... (loop (cdr xs*))))))
            (loop xs)))
    ((for xs as i expr ...)
     (for i in xs expr ...))))
;;6.3
(define-syntax while
  (syntax-rules ()
    ((while condition expr ...)
     (letrec ((function (lambda (condition*)
                      (when condition*
                          (begin expr ... (function condition))))))
       (function condition)))))

;;6.4
(define-syntax repeat
  (syntax-rules (until)
    ((repeat expr ... until condition)
     (while (not condition) expr ...))))
#|
(define-syntax repeat
  (syntax-rules (until)
    ((_ expr1 ... until cond?)
     (letrec ((func (lambda (cnd)
                      (if (not cnd)
                          (let () expr1 ... (func cond?))))))
       (func cond?)))))
|#
#|
(define-syntax repeat
  (syntax-rules (until)
    ((repeat expr ... until condition)
     (letrec ((function (lambda (condition*)
                      (when (not condition*)
                          (begin expr ... (function condition))))))
       (function condition)))))
|#
;;6.5
(define-syntax cout
  (syntax-rules()
    ((cout . exprs)
     (begin
       (define (loop expr)
         (cond ((or (null? expr) (= 1 (length expr))) (display ""))
               ((equal? (car expr) '<<) (loop (cdr expr)))
               ((equal? (car expr) 'endl) (newline) (cdr expr))
               (else (display (car expr)) (loop (cdr expr)))))
       (loop 'exprs)))))

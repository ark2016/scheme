(define e  2.7182818284590452353602874713526624977572)
(define pi 3.1415926535897932384626433832795028841971)

(define (cot x) (expt (tan x) -1))
(define (acot x) (- (/ pi 2) (atan x)))
(define (sh x) (/ (- (exp x) (exp (- x))) 2))
(define (ch x) (/ (+ (exp x) (exp (- x))) 2))
(define (th x) (/ (sh x) (ch x)))
(define (cth x) (/ (ch x) (sh x)))
(define (arcsh x) (log (+ x (expt (+ (expt x 2) 1) .5))))
(define (arcch x) (log (+ x (expt (- (expt x 2) 1) .5))))
(define (arcth x) (log (/ (expt (- 1 (expt x 2)) .5) (- 1 x))))
(define (arccth x) (log (/ (expt (- (expt x 2) 1) .5) (- x 1))))

(define (derivative expression)
  (define (fderiv operation expr)
    (cond ((and (equal? operation '*) (number? (car expr)) (= (length expr) 2))
           (list '* (car expr) (deriv (cadr expr))))

          ((and (equal? operation '*) (number? (car expr)))
           (list '* (car expr) (deriv (cons '* (cdr expr)))))
          
          ((equal? operation '*)
           (list '+ (list '* (deriv (car expr)) (cadr expr))
                 (list '* (car expr) (deriv (cadr expr)))))
      
          ((equal? operation '/)
           (list '/ (list '- (list '* (deriv (car expr))(cadr expr)) (list '* (car expr)(deriv (cadr expr))))
                 (list 'expt (cadr expr) 2)))
      
          ((and (equal? operation 'expt) (equal? (car expr) 'x))
           (list '* (cadr expr) (list 'expt 'x (- (cadr expr) 1))))
      
          ((and (equal? operation 'expt) (number? (car expr)))
           (list '* expr (list 'log (car expr))))
      
          ((equal? operation 'sin)
           (list '* (list 'cos (car expr)) (deriv (car expr))))
      
          ((equal? operation 'cos)
           (list '* (list '-sin (car expr)) (deriv (car expr))))

          ((equal? operation 'tan)
           (list '* (list '/ 1 (list 'expt (list 'cos (car expr)) 2)) (deriv (car expr))))

          ((equal? operation 'cot)
           (list '* (list '/ -1 (list 'expt (list 'sin (car expr)) 2)) (deriv (car expr))))

          ((equal? operation 'asin)
           (list '* (list '/ 1 (list 'expt (list '- 1 (list 'expt (car expr) 2)) .5)) (deriv (car expr))))

          ((equal? operation 'acos)
           (list '* (list '/ -1 (list 'expt (list '- 1 (list 'expt (car expr) 2)) .5)) (deriv (car expr))))

          ((equal? operation 'atan)
           (list '* (list '/ 1 (list '+ 1 (list 'expt (car expr) 2))) (deriv (car expr))))

          ((equal? operation 'acot)
           (list '* (list '/ -1 (list '+ 1 (list 'expt (car expr) 2))) (deriv (car expr))))
      
          ((equal? operation 'exp)
           (list '* (list 'exp (car expr)) (deriv (car expr))))
      
          ((and (equal? operation 'log) (= (length expr) 1));ln(x)
           (list '/ (deriv (car expr)) (car expr)))

          ((equal? operation 'log); x = log a b <=> b ^ x = a <=> log_b(a)
           (list '* (list '/ (deriv (car expr)) (car expr)) (list '/ 1 (cons 'log (cdr expr)))))

          ((equal? operation 'sh)
           (list '* (list 'ch (car expr)) (deriv (car expr))))

          ((equal? operation 'ch)
           (list '* (list 'sh (car expr)) (deriv (car expr))))

          ((equal? operation 'th)
           (list '* (list '/ 1 (list 'expt (list 'ch (car expr)) 2)) (deriv (car expr))))

          ((equal? operation 'cth)
           (list '* (list '/ -1 (list 'expt (list 'sh (car expr)) 2)) (deriv (car expr))))

          ((equal? operation 'arcsh)
           (list '* (list '/ 1 (list 'expt (list '+ (list 'expt (car expr) 2) 1) .5)) (deriv (car expr))))

          ((equal? operation 'arcch)
           (list '* (list '/ 1 (list 'expt (list '- (list 'expt (car expr) 2) 1) .5)) (deriv (car expr))))

          ((or (equal? operation 'arcth) (equal? operation 'arccth))
           (list '* (list '/ 1 (list '- 1 (list 'expt (car expr) 2))) (deriv (car expr))))

          (else 1)))
  
  (define (deriv expr)
    (if (not (list? expr)) (set! expr (list expr)))
    (let ((operation (car expr)))
      (cond((number? operation) 0)
           ((equal? operation 'x) 1)
           (else (fderiv operation (cdr expr))))))

  (define (loop expr)
    (if (null? expr)
        '()
        (cons (deriv (car expr)) (loop (cdr expr)))))
  
  (cond ((null? expression) '())
        ((number? expression) expression)
        ((or (equal? (car expression) '+) (equal? (car expression) '-))
         (let ((signum (car expression)))
           (cons signum (loop (cdr expression)))))
        (else (deriv expression))))


;\_______________________________________________λ*\achive/*λ_______________________________________________/
(define __eval__ (lambda (x) (eval x (interaction-environment))))

(define (my-flatten xs)
  (define (loop xs res)
    (if (null? xs)
        res
        (if (list? (car xs))
            (loop (cdr xs) (loop (car xs) res))
            (loop (cdr xs) (reverse (cons (car xs) (reverse res)))))))
  (loop xs '()))

(define-syntax flatten
  (syntax-rules ()
    ((_ xs)
     (__eval__ (my-flatten xs)))))
#|
(define-syntax mderivative 
  (syntax-rules ()
    ((_ expr)
      (let ((λ  (delay (derivative  expr))))
        (display (force λ))
        (newline)
        (force λ)))))
|#

(define-syntax mderivative 
  (syntax-rules ()
    ((_ expr)
     (__eval__ (derivative (quote expr))))))

;(define s (lambda (x) (__eval__ (derivative '(expt x 3)))))
;((lambda (y) (let ((x y)) ((__eval__ (derivative '(expt x 10))) x))) 10)
;((lambda (y) (let ((x y) (expr (derivative '(expt x 10)))) (define x y) ((__eval__ expr) y))) 10)
(define (der-cube x)
  (mderivative (expt x 3)))

(define (my-flatten xs)
  (define (loop xs res)
    (if (null? xs)
        res
        (if (list? (car xs))
            (loop (cdr xs) (loop (car xs) res))
            (loop (cdr xs) (reverse (cons (car xs) (reverse res)))))))
  (loop xs '()))

(define (simplify xs)
  (define (one-multiply arr res)
    (cond ((and (null? arr) (= 1 (length res))) 1)
          ((null? res) res)
          ((list? (car arr)) (one-multiply (cdr arr) (append res (list (simplify (car arr))))))
          ((and (number? (car arr)) (= 1 (car arr))) (one-multiply (cdr arr) res))
          (else (one-multiply (cdr arr) (append res (list (car arr)))))))
  #|
  (define (zero-sum arr res)
    (cond((and (null? arr) (null? res)) 0)
         ((null? arr) res)
         ((list? (car arr)) (zero-sum (cdr res) (append res (list (simplify (car res))))))
         ((and (number? (car res)) (= 0 (car res))) (zero-sum (cdr arr) res))
         (else (one-multiply (cdr arr) (append res (list (car res)))))))
|#
  (define (zero-sum xs)
    (or (and (>= 3 (length xs))
             (or (and (number? (cadr xs)) (= 0 (cadr xs)) (simplify (caddr xs))) (simplify (caadr xs)))))
    xs)

  
  (define (loop xs res)
    (cond ((null? xs) res)
          ((and (list? (car xs)) (equal? (car xs) '*) (member 0 (car xs)))
           (loop (cdr xs) res))
          ((and (list? (car xs)) (equal? (caar xs) '*) (member 1 (car xs)))
           (loop (cdr xs) (append res (list (one-multiply (car xs) '())))))
          ((and (list? (car xs)) (equal? (caar xs) '+) (member 0 (car xs)))
           (loop (cdr xs) (append res (list (zero-sum (car xs) )))))
          ;((list? (car xs)) (simplify (car xs)))
          (else (loop (cdr xs) (append res (list (car xs)))))))
  (car (loop (list xs) '())))
;;; TESTS
(define-syntax test
  (syntax-rules ()
    ((test call res) (list (quote call) res))))

(define (run-test test)
  (display (car test))
  (define res (eval (car test) (interaction-environment)))
  (if (equal? res (cadr test))
      (begin (display " ok") (newline) #t)
      (begin (display " FAIL") (newline)
             (display "\tExpected: ") (display (cadr test)) (newline)
             (display "\tReturned: ") (display res) (newline)
             #f)))

(define (fand x y) (and x y))
(define (run-tests tests)
  (or (null? tests)
      (fand (run-test (car tests))
            (run-tests (cdr tests)))))

(define derivative-tests
  (list (test (derivative '(2)) '0) ;1
        (test (derivative '(x)) '1) ;2
        (test (derivative '(-x)) '-1) ;3
        (test (derivative '(* 1 x)) '(* 1 1)) ;4
        (test (derivative '(* -1 x)) '(* -1 1)) ;5
        (test (derivative '(* -4 x)) '(* -4 1)) ;6
        (test (derivative '(* 10 x)) '(* 10 1))
        (test (derivative '(- (* 2 x) 3)) '(- (* 2 1) 0)) ;8
        (test (derivative '(expt x 10)) '(* 10 (expt x 9))) ;9
        (test (derivative '(* 2 (expt x 5))) '(* 2 (* 5 (expt x 4)))) ;10
        (test (derivative '(expt x -2)) '(* -2 (expt x -3))) ;11
        (test (derivative '(expt 5 x)) '(* (5 x) (log 5))) ;12
        (test (derivative '(cos x)) '(* (-sin x) 1)) ;13
        (test (derivative '(sin x)) '(* (cos x) 1)) ;14
        (test (derivative '(exp x)) '(* (exp x) 1)) ;15
        (test (derivative '(* 2 (exp x))) '(* 2 (* (exp x) 1))) ;16
        (test (derivative '(* 2 (exp (* 2 x)))) '(* 2 (* (exp (* 2 x)) (* 2 1)))) ;17
        (test (derivative '(log x)) '(/ 1 x)) ;18
        (test (derivative '(* 3 (log x))) '(* 3 (/ 1 x))) ;19
        (test (derivative '(+ (expt x 3) (expt x 2)))
              '(+ (* 3 (expt x 2)) (* 2 (expt x 1)))) ;20
        (test (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2))))
              '(- (* 2 (* 3 (expt x 2))) (* 2 (* 2 (expt x 1))))) ;21
        (test (derivative '(/ 3 x)) '(/ (- (* 0 x) (* 3 1)) (expt x 2))) ;22
        (test (derivative '(* 3/2 (expt x -2))) '(* 3/2 (* -2 (expt x -3)))) ;23
        (test (derivative '(* 2 (sin x) (cos x)))
              '(* 2 (+ (* (* (cos x) 1) (cos x)) (* (sin x) (* (-sin x) 1))))) ;24
        (test (derivative '(* 2 (exp x) (sin x) (cos x)))
              '(* 2 (+ (* (* (exp x) 1) (sin x)) (* (exp x) (* (cos x) 1))))) ;25
        (test (derivative '(sin (* 2 x))) '(* (cos (* 2 x)) (* 2 1))) ;26
        (test (derivative '(cos (* 2 (expt x 2))))
              '(* (-sin (* 2 (expt x 2))) (* 2 (* 2 (expt x 1))))) ;27
        (test (derivative '(sin (log (expt x 2))))
              '(* (cos (log (expt x 2))) (/ (* 2 (expt x 1)) (expt x 2)))) ;28
        (test (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2)))))
              '(+ (* (cos (* 2 x)) (* 2 1)) (* (-sin (* 2 (expt x 2))) (* 2 (* 2 (expt x 1)))))) ;29
        (test (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2)))))
              '(+ (* (* (cos (* 2 x)) (* 2 1)) (cos (* 2 (expt x 2)))) (* (sin (* 2 x)) (* (-sin (* 2 (expt x 2))) (* 2 (* 2 (expt x 1))))))) ; 30
        ))

;(run-tests derivative-tests)

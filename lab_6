(define (++ x) (+ x 1))
(define (-- x) (- x 1))
(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (begin (write 'expr)
            (display " => ")
            (let ((x expr))
              (write x)
              (newline)
              x)))))

(define-syntax test
  (syntax-rules (cons)
    ((_ (fun  arg) res) (list 'fun fun (list arg) res))
    ((_ (fun arg . args) res) (list 'fun fun (cons arg (list . args)) res ))))

(define (run-test test)
  (define (write-list xs)
    (and (not (null? xs))
         (write (car xs))
         (display " ")
         (write-list (cdr xs))))
  (begin
    (display "(")
    (display (car test))
    (display " ")
    (write-list (caddr test))
    (display ") ")
    (let ((val (apply (cadr test) (caddr test)))
          (res (cadddr test)))
      (if (equal? val res)
          (display "ok\n")
          (begin (display "FAIL\n")
                 (display "\tExpected: ")
                 (display res)
                 (display "\n\tReturned: ")
                 (display val)
                 (newline)))
      (equal? val res))))

(define (run-tests test-s)
  (define (loop ind test-s)
    (if (null? test-s)
        ind
        (loop (and (run-test (car test-s)) ind) (cdr test-s))))
  (loop #t test-s))

;;1
(define (check-frac str);> (check-frac "/1") #t not
  (define check-frac-arr (string->list "1234567890-/+"))
  (define chek-number (string->list "1234567890"))
  (define (loop arr flag-+ flag/ flag-num)
    
    (cond((null? arr) #t)
         ((memq (car arr) chek-number)(loop (cdr arr) flag-+ flag/ (++ flag-num)))
         ((and (= (length arr) 1) (memq (car arr) check-frac-arr) (not (memq (car arr) chek-number))) #f)
         ((and (= flag-num 0) (equal? (car arr) #\/)) #f);;loop -> false
         ((and (memq (car arr) check-frac-arr) (not (or (equal? (car arr) #\-) (equal? (car arr)#\/)))) (loop (cdr arr) flag-+ flag/ flag-num))
         ((and (memq (car arr) check-frac-arr) (or (equal? (car arr) #\-) (equal? (car arr) #\+)) (= flag-+ 0 flag/)) (loop (cdr arr) (++ flag-+) flag/ flag-num))
         ((and (memq (car arr) check-frac-arr) (equal? (car arr) #\/) (= flag/ 0)) (loop (cdr arr) flag-+ (++ flag/) flag-num))
         (else #f)))
  (loop (string->list str) 0 0 0))

(define (char->digit ch)
  (- (char->integer ch) (char->integer #\0)))

(define (scan-frac str)
  (define (loop arr up down flag/ sgn)
    #|
    (write "     ")
    (display up)
    (write "/")
    (display down)
    (newline)
    |#
    (cond ((null? arr) (/ up down sgn))
          ((equal? (car arr) #\/) (loop (cdr arr) up down 1 sgn))
          ((equal? (car arr) #\+) (loop (cdr arr) up down flag/ +1))
          ((equal? (car arr) #\-) (loop (cdr arr) up down flag/ -1))
          ((= flag/ 0) (loop (cdr arr) (+ (* up 10)  (char->digit (car arr))) down flag/ sgn))
          (else (loop (cdr arr) up (+ (* down 10) (char->digit (car arr))) flag/ sgn))))
  (and (check-frac str) (loop (string->list str) 0 0 0 1)))

(define (search-f res arr)
  (if (null? arr)
      res
      (if (car arr)
          (search-f res (cdr arr))
          #f)))
      

(define (scan-many-fracs str)
  (define skip '(#\space #\tab #\newline))
  (define (loop arr frac res)
    (cond ((and (not (null? frac)) (null? arr)) (append res (list (scan-frac (list->string frac)))))
          ((null? arr)res)
          ;((equal? (car arr) #\\) (scan-frac (list->string frac)) (loop (cddr arr '())))
          ;((equal? (car arr) #\space) (scan-frac (list->string frac)) (loop (cdr arr)'()))
          ((and (not (null? frac)) (memq (car arr) skip));(write "|")(display (scan-frac (list->string frac)))
           (loop (cdr arr)'() (append res (list (scan-frac (list->string frac))))))
          ((memq (car arr) skip) (loop (cdr arr) frac res))
          (else ;(display (car arr))(newline)
           (loop (cdr arr) (append frac (list (car arr))) res))))
  (let ((arr (loop (string->list str) '() '())))
    (search-f arr arr)))


;;2
(define (parse vec)
  (define __eval__ (lambda (x) (eval x (interaction-environment))))
  ;(define funcs-arr '('+ '- '* 'mod 'neg '= '< '> 'not 'and 'or 'drop 'swap 'dup 'over 'rot 'depth 'if))
  (define funcs-arr '(+ - * mod neg = < > not and or drop swap dup over rot depth))

  (define dict '())


  (define (parse* vec)
    (if (null? (vector->list vec))
        '(()())
        (let ((parsed (parce (vector->list vec) '())))
          (if parsed
              (if (equal? parsed '((if #f)))
                  #f
                  (list dict parsed))
              parsed))))
  
  (define (parce-define arr stack-return)
    (if (null? arr)
        #f
        (if (equal? (car arr) 'end)
            (begin (set! dict (cons  (cons (car stack-return) (list (cdr stack-return)))    dict))
                   (cdr arr))
            (parce-define (cdr arr) (append  stack-return (list (car arr)))))))
  #|
(define (parce-if arr res)
  (if (equal? (car arr) 'endif)
      res
      (parce-if (cdr arr) (cons (car arr) res))))
|#
  (define (parce-if arr res)
    (cond ((null? arr) #f);; arr -> #f
          ((equal? (car arr) 'define) #f)
          ((equal? (car arr) 'if)
           (let ((add-if (parce-if (cdr arr) '())))
             (parce-if (parce-endif (cdr arr)) (append res (list (list 'if add-if))))))
          ((equal? (car arr) 'endif) res)
          (else (parce-if (cdr arr) (append res (list (car arr)))))))
  #|
(define (parce-endif arr)
  (if (equal? (car arr) 'endif)
      (cdr arr)
      (parce-endif (cdr arr))))
|#
  (define (parce-endif arr)
    (cond ((null? arr) '())
          ((equal? (car arr) 'if) (parce-endif (parce-endif (cdr arr))))
          ((equal? (car arr) 'endif);(display arr)(newline)
           (cdr arr))
          (else (parce-endif (cdr arr)))))

  (define (parce arr res)
    
    ;(write res)
    ;(newline)
    (if (not arr)
        arr
        (if (null? arr)
            res
            (let ((fnel (car arr)))
              (cond ((member fnel funcs-arr) (parce (cdr arr) (append res (list fnel))))
                    ((equal? fnel 'define);(display " define ")
                     (cond ((< (length arr) 3) #f)
                           ((> (length arr) 3) (parce (parce-define (cdr arr) '()) res))
                           ((and (not (equal? (cadr arr) 'endif)) (equal? (caddr arr) 'end)) (set! dict (cons  (cons (cadr arr) '(()))    dict)) (parce (cdddr arr) res))
                           (else #f)))
                    ;((equal? fnel 'if) (display " if ") (parce (parce-endif arr 0) (append res (list parce-if (cdr arr) 0 '()))))
                    ((equal? fnel 'if) ;(display " if ")
                     (let ((add-if (parce-if (cdr arr) '())))
                       (parce (parce-endif arr) (append res (list (list 'if add-if))))))
                    ((number? fnel) (parce (cdr arr) (append res (list fnel))))
                    ((equal? fnel 'end) #f)
                    ((equal? fnel 'endif) #f)
                    ((assoc fnel dict) (parce (cdr arr) (append res (list fnel))));;x можнo?
                    (else ;(write fnel) (newline)
                     (parce (cdr arr) (append res (list fnel)))))))))
  (parse* vec))
(parse #(x dup 0 swap if drop -1 endif 1))

(define second-tests
  (list (test (parse #(1 2 +)) '(() (1 2 +)))

        (test (parse #(x dup 0 swap if drop -1 endif))
              '(() (x dup 0 swap (if (drop -1)))))
        (test (parse #( define -- 1 - end
                         define =0? dup 0 = end
                         define =1? dup 1 = end
                         define factorial
                         =0? if drop 1 exit endif
                         =1? if drop 1 exit endif
                         dup --
                         factorial
                         *
                         end
                         0 factorial
                         1 factorial
                         2 factorial
                         3 factorial
                         4 factorial )) '(((-- (1 -))
                                           (=0? (dup 0 =))
                                           (=1? (dup 1 =))
                                           (factorial
                                            (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
                                          (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial)))
        (test (parse #(define word w1 w2 w3)) #f)
        (test (parse #(   define =0? dup 0 = end
                           define gcd
                           =0? if drop exit endif
                           swap over mod
                           gcd
                           end
                           90 99 gcd
                           234 8100 gcd    )) '(((=0? (dup 0 =)) (gcd (=0? (if (drop exit)) swap over mod gcd))) (90 99 gcd 234 8100 gcd)))
        (test (parse #(if define a end endif)) #f)
        (test (parse #(define a define b 1 end end)) #f)
        (test (parse #(define a if end endif)) #f)
        (test (parse #(define end)) #f)
        (test (parse #()) '(()()))
        (test (parse #(define endif end)) #f)
        (test (parse #(define a)) #f)
        ))
(run-tests second-tests)

#|

(define (scan-frac str)
  (define check-frac-arr (string->list "1234567890-/+"))
  (define frac 0)
  (define quot 0)
  (define sgn +)
  (define (loop listr aft)
    (cond
      ((null? listr) #t)
      ((memq (car listr) check-frac-arr)
       (cond
         ((equal? (car listr)#\/) (loop (cdr listr) 1))
         ((equal? (car listr)#\+) (loop (cdr listr) aft))
         ((and  (equal? (car listr) #\-) (= aft 0)) (set! sgn -) (loop (cdr listr) aft))
         ((and  (equal? (car listr) #\-) (= aft 1)) #f)
         (else
          (or
           (and (equal? aft 0) (set! frac (sgn (* frac 10)
                                               (char->digit (car listr)))))
           (and (equal? aft 1) (set! quot (+ (* quot 10)
                                             (char->digit (car listr))))))
          (loop (cdr listr) aft))))
      (else #f)))
  (and (loop (string->list str) 0)
       (not (= quot 0)) ;; не знаю, надо ли проверять деление на 0
       (/ frac quot))) ;; выводить через write! display выводит целую часть из дробей, нам этого не надо!
|#

(load "unit-test.scm")
;
(define char-letter? char-alphabetic?)
(define char-digit? char-numeric?)
(define call/cc call-with-current-continuation)
(define e #\e)
(define point #\.)
;
(define (str-to-sym-to-str s)
  (or (and (symbol? s) (symbol->string s)) (string->symbol s)))
;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Запрос первых двух символов
(define (peek2 stream)
  (if (null? (car stream))
      (cadr stream)
      (if (null? (cdar stream))
          (list (caar stream))
          (list (caar stream) (cadar stream)))))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))
;;1. Лексический анализатор
(define parentheses '(#\( #\)))
(define arithmetic-operations '(#\+ #\- #\* #\/ #\^))
(define +- '(+ -))
;
(define (tokenize str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))
    
    (call/cc
     (lambda (error)
       (define result (tokens stream error))
       (and (equal? (peek stream) EOF)
            result)))))
;
(define (tokens stream error)
  (define (start-token? char)
    (or (char-letter? char)
        (char-digit? char)
        (operator? char)
        (member char parentheses)))
  (cond ((char-whitespace? (peek stream))
         (spaces stream error)
         (tokens stream error))
        ((start-token? (peek stream))
         (cons (token stream error)
               (tokens stream error)))
        (else '())))
;
(define (operator? char)
  (member char arithmetic-operations))
;
(define (spaces stream error)
  (cond ((char-whitespace? (peek stream))
         (next stream))
        (else #t)))
;
(define (token stream error)
  (cond ((member (peek stream) parentheses)
         (string (next stream)))
        ((operator? (peek stream))
         (string->symbol (string (next stream))))
        ((char-letter? (peek stream))
         (variable-or-keyword stream error))
        ((char-digit? (peek stream))
         (digit stream error))
        (else (error #f))))
;
(define (variable-or-keyword stream error)
  (cond ((char-letter? (peek stream))
         (str-to-sym-to-str (list->string (cons (next stream) (variable-tail stream error)))))
        (else (error #f))))
;
(define (variable-tail stream error)
  (cond ((char-letter? (peek stream))
         (cons (next stream) (variable-tail stream error)))
        ((char-digit? (peek stream))
         (cons (next stream) (variable-tail stream error)))
        (else '())))
;
(define (digit stream error)
  (cond ((char-digit? (peek stream))
         (string->number (list->string (cons (next stream)(number-tail stream error)))))
        (else (error #f))))
;
(define (number-tail stream error)
  (cond ((char-digit? (peek stream))
         (cons (next stream) (variable-tail stream error)))
        ((equal? (peek stream) point)
         (cons (next stream) (point-tail stream error)))
        ((and (equal? (peek stream) e) (next-digit+-? stream))
         (cons (next stream) (exp-tail stream error)))
        (else '())))
;
(define (next-digit+-? stream)
  (or (char-digit? (peek2 stream))
      (member (peek2 stream) +-)))
;
(define (point-tail stream error)
  (cond ((char-digit? (peek stream))
         (cons (next stream) (variable-tail stream error)))
        ((and (equal? (peek stream) e) (next-digit+-? stream))
         (cons (next stream) (exp-tail stream error)))
        (else '())))
;
(define (exp-tail stream error)
  (cond((char-digit? (peek stream))
        (cons (next stream) (rec-exp-tail stream error)))
       ((member (peek stream) +-)
        (cons (next stream) (rec-exp-tail stream error)))
       (else '())))
;
(define (rec-exp-tail stream error)
  (cond ((char-digit? (peek stream))
         (cons (next stream) (rec-exp-tail stream error)))
        (else '())))
;;2. Синтаксический анализатор
#|
Expr    ::= Term Expr' .
Expr'   ::= AddOp Term Expr' | .
Term    ::= Factor Term' .
Term'   ::= MulOp Factor Term' | .
Factor  ::= Power Factor' .
Factor' ::= PowOp Power Factor' | .
Power   ::= value | "(" Expr ")" | unaryMinus Power .
|#
(define operation '(- + * / ^))
(define */ '(* /))
;
(define (parse xs)
  (let* ((EOF (integer->char 0))
         (stream (make-stream  xs EOF)))
    (call/cc
     (lambda (error)
       (define result (tokens2 stream error))
       (and (equal? (peek stream) EOF)
            result)))))
;
(define (tokens2 stream error)
  (cond ((equal? (peek stream) #\nul) #f)
        ((start-term? (peek stream))
         (let* ((pow (term stream error))
                (tail (rec-expr stream error  pow)))
           (if (null? tail)
               pow
               tail)))
        (else  #f)))
;
(define (start-term? token)
  (and (or (not (member token operation)) (equal? token '+)) (not (null? token))))
;
(define (term stream error)
  (let* ((pow (factor stream error)) (tail (rec-term stream error  pow)))
    (if (null? tail)
        pow
        tail)))
;
(define (factor stream error)
       
  (let (( pow (power stream error)) (tail (degree stream error)))
    (if (null? tail)
        pow
        (cons pow tail))))
;
(define (power stream error)
  (cond ((string? (peek stream))
         (next stream)
         (let ((expr (tokens2 stream error)))
           (or (and (equal? (next stream) ")") (tokens2 stream error)) (error #f))))
        ((equal? (peek stream) '-)
         (list (next stream) (power stream error)))
        (else (next stream))))
;
(define (degree stream error)
  (if (equal? (peek stream) '^)
      (list (next stream)  (factor stream error))
      '()))
;
(define (rec-term stream error res )
  (if (member (peek stream) */)
      (rec-term stream error (list res  (next stream)  (factor stream error)))
      res))
;
(define (rec-expr stream error res)
  (let ((token (peek stream)))
    (cond ((member (peek stream) +-)
          (next stream)
          (or (and (equal? (peek stream) #\nul) (error #f))
              (rec-expr stream error (list res  token  (term stream error)))))
        (else res))))
;;3. Преобразователь дерева разбора в выражение на Scheme
(define (tree->scheme parsed)
  (if (list? parsed)
      (if (> (length parsed) 2)
          (if (equal? (cadr parsed) '^)
              (list 'expt (tree->scheme(car parsed)) (tree->scheme(caddr parsed)))
              (list (cadr parsed) (tree->scheme(car parsed)) (tree->scheme(caddr parsed))))
          parsed)
      parsed))
;
(define tests
  (list (test (tokenize "1") '(1))
        (test (tokenize "-a") '(- a))
        (test (tokenize "-a + b * x^2 + dy") '(- a + b * x ^ 2 + dy))
        (test (tokenize "(a - 1)/(b + 1)") '("(" a - 1 ")" / "(" b + 1 ")"))
        (test (parse (tokenize "a/b/c/d")) '(((a / b) / c) / d))
        (test (parse (tokenize "a^b^c^d")) '(a ^ (b ^ (c ^ d))))
        (test (parse (tokenize "a/(b/c)")) '(a / (b / c)))
        (test (parse (tokenize "a + b/c^2 - d")) '((a + (b / (c ^ 2))) - d))
        (test (tree->scheme (parse (tokenize "x^(a + 1)"))) '(expt x (+ a 1)))
        (test (eval (tree->scheme (parse (tokenize "2^2^2^2"))) (interaction-environment)) 65536)))
(run-tests tests)

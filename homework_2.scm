(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (begin (write 'expr)
            (display " => ")
            (let ((x expr))
              (write x)
              (newline)
              x)))))
;;1. Обработка списков
(define (my-range a b d)
  (define (loop a b d res x)
    (if (or (and (>= a (- b d)) (> d 0)) (and (<= a (- b d)) (< d 0)))
        res
        (loop (+ a d) b d (append res (list (+ x d))) (+ x d))))
  (loop a b d (list a) a))
;
(define (my-flatten xs)
  (define (loop xs res)
    (if (null? xs)
        res
        (if (list? (car xs))
            (loop (car xs) res)
            (loop (cdr xs) (append res (list (car xs)))))))
  (define (iter xs res)
    (if (null? xs)
        res
        (if (list? (car xs))
            (iter (cdr xs) (loop xs res))
            (iter (cdr xs) (append res (list (car xs)))))))
  (iter xs '()))
;
(define (my-element? x xs)
  (if (null? xs)
      (not (null? xs));#f
      (if (equal? x (car xs))
          (equal? x (car xs))
          (my-element? x (cdr xs)))))
;
(define (my-filter pred? xs)
  (define (loop pred? xs res)
    (if (null? xs)
        res
        (if (pred? (car xs))
            (loop pred? (cdr xs) (append res (list (car xs))))
            (loop pred? (cdr xs) res))))
  (loop pred? xs '()))
;
(define (my-fold-left op xs)
  (define (loop op xs res)
    (if (null? xs)
        res
        (loop op (cdr xs) (op res (car xs)))))
  (if (null? xs)
      xs
      (if (= (length xs) 1)
          (car xs)
          (loop op (cdr xs) (car xs)))))
;
(define (my-fold-right op xs)
  (my-fold-left op (reverse xs)))
#|
(my-range  0 11 3) ⇒ (0 3 6 9)

(my-flatten '((1) 2 (3 (4 5)) 6)) ⇒ (1 2 3 4 5 6)

(my-element? 1 '(3 2 1)) ⇒ #t
(my-element? 4 '(3 2 1)) ⇒ #f

(my-filter odd? (my-range 0 10 1)) ⇒ (1 3 5 7 9)
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)) ⇒ (0 3 6 9 12)

(my-fold-left  quotient '(16 2 2 2 2)) ⇒ 1
(my-fold-left  quotient '(1))          ⇒ 1
(my-fold-right expt     '(2 3 4))      ⇒ 2417851639229258349412352
(my-fold-right expt     '(2))          ⇒ 2
|#
;;2. Множества
(define (list->set xs)
  (define (loop xs res)
    (if (null? xs)
        res
        (if (my-element? (car xs) res)
            (loop (cdr xs) res)
            (loop (cdr xs) (append res (list (car xs)))))))
  (loop xs '()))
;
(define (set? xs)
  (equal? xs (list->set xs)))
;
(define (union xs ys)
  (list->set (append xs ys)))
  
;
(define (intersection xs ys)
  (define (loop xs ys res)
    (if (null? xs)
        res
        (if (my-element? (car xs) ys)
            (loop (cdr xs) ys (append res (list (car xs))))
            (loop (cdr xs) ys res))))
  (loop xs ys '()))
;
(define (difference xs ys)
  (define (loop xs ys res)
    (if (null? xs)
        res
        (if (my-element? (car xs) ys)
            (loop (cdr xs) ys res)
            (loop (cdr xs) ys (append res (list (car xs)))))))
  (loop xs ys '()))
;
(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))
;
(define (set-eq? xs ys)
  (and (equal? xs (intersection xs ys)) (equal? xs (union xs ys))))
#|
(list->set '(1 1 2 3))                       ⇒ (3 2 1)
(set? '(1 2 3))                              ⇒ #t
(set? '(1 2 3 3))                            ⇒ #f
(set? '())                                   ⇒ #t
(union '(1 2 3) '(2 3 4))                    ⇒ (4 3 2 1)
(intersection '(1 2 3) '(2 3 4))             ⇒ (2 3)
(difference '(1 2 3 4 5) '(2 3))             ⇒ (1 4 5)
(symmetric-difference '(1 2 3 4) '(3 4 5 6)) ⇒ (6 5 2 1)
(set-eq? '(1 2 3) '(3 2 1))                  ⇒ #t
(set-eq? '(1 2) '(1 3))                      ⇒ #f
|#
;;3. Работа со строками
(define (string-trim-left str)
  (define (loop xs)
    (if (char-alphabetic?(car xs))
        (list->string xs)
        (loop (cdr xs))))
  (loop (string->list str)))
;
(define (string-trim-right str)
  (define (loop xs)
    (if (char-alphabetic?(car xs))
        (list->string (reverse xs))
        (loop (cdr xs))))
  (loop (reverse (string->list str))))
;
(define (string-trim  str)
  (string-trim-right (string-trim-left str)))
;
(define (string-prefix? a b)
  (define (loop ax bx)
    (if (null? bx)
        (not (null? bx));#f
        (if (null? ax)
            (null? ax);#t
            (if (equal? (car ax) (car bx))
                (loop (cdr ax) (cdr bx))
                (equal? (car ax) (car bx))))));#f
  (loop (string->list a) (string->list b)))
;
(define (string-suffix? a b)
  (define (loop ax bx)
    (if (null? bx)
        (not (null? bx));#f
        (if (null? ax)
            (null? ax);#t
            (if (equal? (car ax) (car bx))
                (loop (cdr ax) (cdr bx))
                (equal? (car ax) (car bx))))));#f
  (loop (reverse (string->list a)) (reverse (string->list b))))
;
(define (string-infix? a b)
  (if (string-suffix? a b)
      (string-suffix? a b)
      (if (= (string-length b) 0)
          (not (= (string-length b) 0));#f
          (if (string-prefix? a b)
              (string-prefix? a b)
              (string-infix? a (list->string (cdr (string->list b))))))))
;
(define (string-split str sep)
  (define (delete xs sep)
    (if (null? sep)
        (list->string xs)
        (delete (cdr xs) (cdr sep))))
  (define (loop str sep res sub-res)
    (if (= (string-length str) 0)
        (append res (list sub-res))
        (if (string-prefix? sep str)
            (loop (delete (string->list str) (string->list sep)) sep (append res (list sub-res)) "")
            (loop (list->string (cdr (string->list str))) sep res (string-append sub-res (substring str 0 1))))))
  (if (and (string-infix? sep str) (not (equal? sep "")))
      (loop str sep '() "")
      (list str)));#f
#|
(string-trim-left  "\t\tabc def")   ⇒ "abc def"
(string-trim-right "abc def\t")     ⇒ "abc def"
(string-trim       "\t abc def \n") ⇒ "abc def"

(string-prefix? "abc" "abcdef")  ⇒ #t
(string-prefix? "bcd" "abcdef")  ⇒ #f
(string-prefix? "abcdef" "abc")  ⇒ #f

(string-suffix? "def" "abcdef")  ⇒ #t
(string-suffix? "bcd" "abcdef")  ⇒ #f

(string-infix? "def" "abcdefgh") ⇒ #t
(string-infix? "abc" "abcdefgh") ⇒ #t
(string-infix? "fgh" "abcdefgh") ⇒ #t
(string-infix? "ijk" "abcdefgh") ⇒ #f
(string-infix? "bcd" "abc")      ⇒ #f

(string-split "x;y;z" ";")       ⇒ ("x" "y" "z")
(string-split "x-->y-->z" "-->") ⇒ ("x" "y" "z")
|#
;;4. Многомерные вектора
#|
(define (ref arg . index-new) ;;; индекс и замена элемента по нему же, с возвратом того же типа
  (define (transform-in-list arg)
    (if (string? arg)
        (string->list arg)
        (if (vector? arg)
            (vector->list arg)
            arg)))
  (define (func-list->type? arg)
    (if (string? arg)
        (string->list)
        (if (vector? arg)
            (vector->list)
            (lambda (x . xs) (car(append (list x) xs))))))
  (define (ref-1 xs index)
    (if (> index (- (length xs) 1))
        (< index (length xs));#f
        (if (= index 0)
            (car xs)
            (ref-1 (cdr xs) (- index 1)))))
  (define (ref-2 xs index replace list->old-type res)
    (if (> index (- (length xs) 1))
        (< index (length xs));#f
        (if (= index 0)
            (list->old-type (append res (list replace) (cdr xs)))
            (ref-2 (cdr xs) (- index 1) replace list->old-type (append res (list (car xs)))))))
  (if (and (not (list? arg)) (not (vector? arg)) (not (string? arg)))
      (display "wrong input")
      (if ( = (length index-new) 1)
          (ref-1 (transform-in-list arg) (car index-new))
          (if (pair? index-new)
              (ref-2 (transform-in-list arg) (car index-new) (car (cdr index-new)) (func-list->type? arg) '())
              (display "Wrong input")))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-multi-vector sizes . fill)
  (define (list-multiply arr res)
    (if (null? arr)
        res
        (list-multiply (cdr arr) (* res (car arr)))))
  (define (multi-vector sizes)
    (make-vector (list-multiply sizes 1)))
  (define (filled-multi-vector sizes element res)
    (make-vector (list-multiply sizes 1) element))
  (if (null? fill)
      (list 'multi-vector sizes fill (multi-vector sizes))
      (list 'multi-vector sizes fill (filled-multi-vector sizes (car fill) 1))))
;
(define (multi-vector? m)
  (equal? (car m) 'multi-vector))
;
(define (multi-vector-ref m indices)
  (define (my-vector-ref vec index)
    (if (= index 0)
        (car vec)
        (my-vector-ref (cdr vec) (- index 1))))
  (define (list-multiply arr res)
    (if (null? arr)
        res
        (list-multiply (cdr arr) (* res (car arr)))))
  (define (multi-index i res)
    (if (= (length i) 1)
        (+ res (car i))
        (multi-index (cdr i) (+ res (list-multiply i 1)))))
  (if (> (multi-index indices 0) (multi-index (car (cdr m)) 0))
      'multi-vector-index-out-of-range
      (my-vector-ref (vector->list (car (cdddr m))) (multi-index indices 0))))
;
(define (multi-vector-set! m indices x)
  (define (multi-index i res)
    (if (= (length i) 1)
        (+ res (car i))
        (multi-index (cdr i) (+ res (list-multiply i 1)))))
  (define (list-multiply arr res)
    (if (null? arr)
        res
        (list-multiply (cdr arr) (* res (car arr)))))
  (if (> (multi-index indices 0) (multi-index (car (cdr m)) 0))
      'multi-vector-index-out-of-range
      (vector-set! (car (cdddr m)) (multi-index indices 0) x )))
#|
(define (multi-vector-set! m indices x)
  (define (multi-index i res)
    (if (= (length i) 1)
        (+ res (car i))
        (multi-index (cdr i) (+ res (list-multiply i 1)))))
  (define (list-multiply arr res)
    (if (null? arr)
        res
        (list-multiply (cdr arr) (* res (car arr)))))
  (define (my-vector-set! head index element tail)
    (write index)
    (write '_)
    (if (> (+ index 1) (length head))
        (begin (display "multi-vector index out of range") (newline))
        (if (= index 0)
            (list->vector (append tail (list element) (cdr head)))
            (my-vector-set! (cdr head) (- index 1) element (append tail (cdr head))))))
  (list (car m) (car (cdr m)) (car (cddr m))
        (my-vector-set! (vector->list (car (cdddr m))) (multi-index indices 0) x '()))); долго считает может можно vector-set! ?
|#
#|
(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m)
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12)) ⇒ test

; Индексы '(1 2 1 1) и '(2 1 1 1) — разные индексы
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1)) ⇒ X
(multi-vector-ref m '(2 1 1 1)) ⇒ Y

(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0)) ⇒ -1
|#
;;5
;(define (o . funcs)
;  (lambda (x) ((my-fold-left (lambda (a . b) (a (lambda (c) ((car b) c)))) funcs) x)))
(define (o . funcs)
  (define (loop reversed-funcs res)
    (if (null? reversed-funcs)
        res
        (loop (cdr reversed-funcs) (lambda (x) ((car reversed-funcs) (res x))))))
  (loop (reverse funcs) (lambda (x) x)))
#|
((o f g h) 1) ⇒ -1
((o f g) 1)   ⇒ 5
((o h) 1)     ⇒ -1
((o) 1)       ⇒ 1
|#
#|
(define (make-multi-vector sizes . fill)
  (define (full num elem res)
    (if (= num 0)
        res
        (full (- num 1) elem (append res (list elem)))))
  (define (vector-size sizes res)
    (if (null? sizes)
        res
        (full (car sizes) (vector-size (cdr sizes) '()) '())))
  (define (vector-fill sizes  fill)
    (if (null? sizes)
        fill
        (full (car sizes) (vector-fill (cdr sizes) fill ) '())))
  (if (null? fill)
      (list->vector (car (vector-size sizes '())))
      (list->vector (vector-fill sizes (car fill) ))))
;
(define (multi-vector? m)
  (and (vector? m) (list? (car (vector->list m)))))
;
(define (multi-vector-ref m indices)
  (if (= 1 (length indices))
      (if (ref m (car indices))
          (ref m (car indices))
          '())
      (multi-vector-ref (ref m (car indices)) (cdr indices))))
;
(define (multi-vector-set! m indices x)
  (define (iter xs)
    (append (list (- (car xs) 1)) (cdr xs)))
  (define (loop m index x)
    (if (or (>= (car index) (length m)) (and (> (length index) 2) (not (list? m))))
        '()
        (if (= (car index) 0)
            (if (> (length index) 2)
                (loop (car m) (cdr index) x)
                (list x))
            (loop (cdr m) (iter index) x))))
  (list->vector (loop (vector->list m) indices x)))
|#          

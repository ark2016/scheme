;;1
(define (count x xs)
  (if (null? xs)
      0
      (if (eqv? x (car xs))
          (+ 1 (count x (cdr xs)))
          (+ 0 (count x (cdr xs))))))
;;2
(define (delete pred? xs)
  (define (loop pred? xs res)
    (if (null? xs)
        res
        (if (pred? (car xs))
            (loop pred? (cdr xs) res)
            (loop pred? (cdr xs) (append res (list(car xs)))))))
  (loop pred? xs '()))
;;3
(define (iterate f x n)
  (define (loop f res n)
    (if (= n 0)
        res
        (loop f (cons x (map f res)) (- n 1))))
  (if (= n 0)
      '()
      (loop f (list x) (- n 1)))) 
;;4
(define (intersperse e xs)
  (define (loop e xs res)
        (if (or (null? xs) (= (length xs) 1))
            (append res xs)
            (loop e (cdr xs) (append res (list (car xs)) (list e)))))
  (loop e xs (list)))
;;5
(define (any? pred? xs)
  (and (not (null? xs)) (or (pred? (car xs)) (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (or (null? xs) (and (not (null? xs)) (and (pred? (car xs)) (any? pred? (cdr xs))))))
;;6
(define (o . funcs)
  (define (loop reversed-funcs res)
    (if (null? reversed-funcs)
        res
        (loop (cdr reversed-funcs) (lambda (x) ((car reversed-funcs) (res x))))))
  (loop (reverse funcs) (lambda (x) x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

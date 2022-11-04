(define e 2,71828182845904523536028747135266249775724709369995)
(define (derivative expr)
;  (define (cut arr)
;    (cond ((number? (car arr)
  (define (separator arr sub-res res);arr arr arr
    (cond ((or (equal? (car arr) #\() (equal? (car arr) #\)) (equal? (car arr) " "))
           (if (equal? sub-res '())
               (separator (cdr arr) '() (append res (list (car arr))))
               (separator (cdr arr) '() (append res sub-res (list (car arr))))))
          ((number? (car arr))
           (separator (cdr arr) (append sub-res (car arr)) res))
          ((cos?))
          ((sin?))
          ((ln?))
          ((expt?))
          ((log?))
          ((
          
  (define (no-change? x)
    (or (number? x) (equal? x #\()(equal? x #\)) (equal? x " "))
  (define (loop arr res)
    (if (null? arr)
        res
        (cond ((no-change? (car arr)) ((loop (cdr arr) (append res (list (car  arr))))))
              ((
        ;(loop (cut (string->list str)) (string-append res)))
  (loop (string->list (quote expr)) '()))

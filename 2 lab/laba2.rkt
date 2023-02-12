(define (count x xs)
  (if (null? xs)
      0
      (if (equal? x (car xs))
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))))




(define (delete pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))
          



(define (iterate f x n)
  (if (= n 0)
      '()
      (cons x (iterate f (f x) (- n 1)))))


(define (intersperse e xs)
  (if (<= (length xs) 1)
      xs
      (cons (car xs)(cons e (intersperse e (cdr xs))))))



(define (any? pred? xs)
  (if (null? xs)
      #f
      (or (pred? (car xs)) (any? pred? (cdr xs)))))


(define (all? pred? xs)
  (if (null? xs)
      #t
      (and (pred? (car xs)) (any? pred? (cdr xs)))))
;6

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))



(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x))))) 
 


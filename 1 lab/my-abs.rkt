(define (my-abs x)
  (if (> x 0)
      x
      (if (< x 0)
          (- x)
          0)))

(define (my-odd? x)
  (= 1 (remainder x 2)))

(define (my-even? x)
  (= 0 (remainder x 2)))

(define (power b e)
  (if (= e 0)
      1
      (if (= e 1)
          b
          (* (power b (- e 1)) b))))

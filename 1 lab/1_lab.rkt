(define pi (* 4 (atan 1)))
(define (circ-len r) (* 2 pi r))
(define (circ-area r) (* pi r r ))
(define (rect-area a b) (* a b))
(define (rect-len a b) (+ a a b b ))
(define (<> a b) (not (= a b)))



(define (signum x)
  (if (> x 0) +1
      (if (< x 0) -1
          0)))

(define (! n)
  (if (> n 0)
      (* (! (- n 1)) n)
      1))

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





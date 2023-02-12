(define (to-bin expr)
  (define sign (car expr))
  (let loop ((l-in (cdr expr)))
    (if (> (length l-in) 2)
        (cons sign (cons (car l-in) (list (loop (cdr l-in)))))
        (if (= (length expr) 3)
            expr
            `(,sign ,(car l-in) ,(cadr l-in))))))
  
  

(define (derivative expr)
  (define head (if (list? expr)
                   (car expr)
                   expr))
  (cond
    ((equal? head 'x) 1)
    ((number? head) 0)
    
    ((equal? head 'expt) (if (number? (caddr expr))
                             `(* ,(derivative (cadr expr)) (* ,(caddr expr) (expt ,(cadr expr) ,(- (caddr expr) 1))))
                             `(* ,(derivative (caddr expr)) (* (log ,(cadr expr)) (expt ,(cadr expr) ,(caddr expr))))))
    ((equal? head 'exp) `(* ,(derivative (cadr expr)) (exp ,(cadr expr))))
    ((equal? head 'sin) `(* ,(derivative (cadr expr)) (cos ,(cadr expr))))
    ((equal? head 'cos) `(* -1 (* ,(derivative (cadr expr)) (sin ,(cadr expr)))))
    ((equal? head 'log) `(/ ,(derivative (cadr expr)) ,(cadr expr)))
    ((equal? head '+) `(+ ,(derivative (cadr (to-bin expr))) ,(derivative (caddr (to-bin expr)))))
    ((equal? head '*) (let ((bin (to-bin expr)))
                        `(+ (* ,(derivative (cadr bin)) ,(caddr bin)) (* ,(cadr bin) ,(derivative (caddr bin))))))
    ((equal? head '-) (if (= (length expr) 2)
                          (list '- (derivative (cdr expr)))
                          (let ((bin (to-bin expr)))
                            `(- ,(derivative (cadr bin)) ,(derivative (caddr bin))))))
    ((equal? head '/) `(/ (- (* ,(derivative (cadr expr)) ,(caddr expr)) (* ,(cadr expr) ,(derivative (caddr expr)))) (expt ,(caddr expr) 2)))
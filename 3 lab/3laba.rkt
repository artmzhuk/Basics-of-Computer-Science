;==========================1
(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex obj)
     (begin
       (display 'obj )
       (display ' =>)
       (display obj)
       (newline)
       obj
       ))))

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) 
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))


;==========================2


(define-syntax test
  (syntax-rules ()
    ((test fx y) '(fx y))))
      
(define (run-tests xs)
  (if (not(null? xs))
      (write (caar xs))
      (and))
  (if(not(null? xs))
     (if (equal? (eval (caar xs) (interaction-environment))
                 (cadar xs))
         (begin
           (write  ' ok)
           (newline)
           (run-tests (cdr xs)))
         (begin
           (write 'FAIL)
           (newline)
           (write 'Expected: )
           (write (eval (caar xs) (interaction-environment)))
           (newline)
           (write 'Returned: )
           (write (cadar xs))
           (newline)
           (run-tests (cdr xs))
           (or)))
     (and)))
        

(define (run-test xs)
  (run-tests (cons xs '())))


(define (signum x) 
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))



;==========================3

(define (ref-list xs n)
  (if  (and (list? xs) (> (length xs) n))
       (list-ref xs n)
       (if (and (vector? xs) (> (vector-length xs) n))
           (vector-ref xs n)
           (and (and (string? xs) (> (string-length xs) n))
                (string-ref xs n)))))




(define (list-head xs n)
  (reverse (list-tail (reverse xs) (- (length xs) n))))

(define (ins-list xs n k) 
  (if (list? xs)
      (if (> (length xs) n)
          (append (list-head xs n) k (list-tail xs n))
          (and (= (length xs) n)
               (cons xs k)))
      (if (vector? xs)
          (if (> (vector-length xs) n)
              (list->vector (append (list-head (vector->list xs) n) k (list-tail (vector->list xs) n)))
              (and (= (vector-length xs) n)
                   (list->vector (cons (vector->list xs) k))))
          (if (string? xs)
              (if (and (> (string-length xs) n)
                       (char? (list-ref (append (list-head (string->list xs) n)
                                                k
                                                (list-tail (string->list xs) n))
                                        n)))
                  (list->string (append (list-head (string->list xs) n)
                                        k
                                        (list-tail (string->list xs) n)))
                  (and (and (= (string-length xs) n)
                            (char? (list-ref (append (list-head (string->list xs) n)
                                                     k
                                                     (list-tail (string->list xs) n)) n)))
                       (list->string (append (string->list xs) k))))))))

(define (ref . p)
  (if (= (length p) 2)
      (ref-list (car p) (cdr p))
      (ins-list (car p) (cadr p) (cddr p))))

;==========================4


(define (factorize expr)
  (if (2-? expr)
      (2- expr)
      (if (3-? expr)
          (3- expr)
          (if (3+? expr)
              (3+ expr)
              (display "error\n")))))

(define (2-? expr)
  (and (list? expr)
       (equal? '- (list-ref expr 0))
       (list? (list-ref expr 1))
       (list? (list-ref expr 2))
       (equal? 'expt (list-ref (list-ref expr 1) 0))
       (equal? '2 (list-ref (list-ref expr 1) 2))))
  
(define (2- expr)
  (let ((a (cadadr expr))
        (b (cadadr (cdr expr))))
    `(* (- ,a ,b)
        (+ ,a ,b))))


(define (3-? expr)
  (and (list? expr)
       (equal? '- (list-ref expr 0))
       (list? (list-ref expr 1))
       (list? (list-ref expr 2))
       (equal? 'expt (list-ref (list-ref expr 1) 0))
       (equal? '3 (list-ref (list-ref expr 1) 2))))
  
(define (3- expr)
  (let ((a (cadadr expr))
        (b (cadadr (cdr expr))))
    `(* (- ,a ,b)
        (+ (* ,a ,a) (* ,a ,b) (* ,b ,b)))))


(define (3+? expr)
  (and (list? expr)
       (equal? '+ (list-ref expr 0))
       (list? (list-ref expr 1))
       (list? (list-ref expr 2))
       (equal? 'expt (list-ref (list-ref expr 1) 0))
       (equal? '3 (list-ref (list-ref expr 1) 2))))
  
(define (3+ expr)
  (let ((a (cadadr expr))
        (b (cadar(cddr expr))))
    `(* (+ ,a ,b)
        (+ (- (* ,a ,a) (* ,a ,b)) (* ,b ,b)))))

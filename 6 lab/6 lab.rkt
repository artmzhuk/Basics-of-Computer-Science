;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(load "unit-test.scm")

(define (parse tokens)
  (load "stream.scm")
  (define (expect stream term error)
    (if (equal? (peek stream) term)
        (next stream)
        (error #f)))
  (define (program stream error)
    (let* ((t-articles (articles stream error))
           (t-body (body stream error)))
      (list t-articles t-body)))
  (define (articles stream error)
    (cond ((eqv? 'define (peek stream))
           (let* ((t-article (article stream error))
                  (t-articles (articles stream error)))
             (cons t-article t-articles)))
          (else '())))
  (define (article stream error)
    (let* ((t-pred (peek stream))
           (t-define (expect stream 'define error))
           (t-word (next stream))
           (t-body (body stream error))
           (t-end (expect stream 'end error)))
      (begin (if (and (or (equal? t-word 'define)
                          (equal? t-word 'endif)
                          (equal? t-word 'exit)
                          (equal? t-word 'end))
                      (equal? t-pred 'define))
                 (error #f))
             (list t-word t-body))))
  (define (body stream error)
    (cond ((eqv? 'if (peek stream))
           (let ((strn stream))
             (if (or (equal? (cadar strn) 'define)
                     (equal? (cadar strn) 'end))
                 (error #f)
                 (let* (
                        (t-if (next stream))
                        (t-body (body stream error))
                        (t-endif (expect stream 'endif error))
                        (t-body-tail (body stream error)))
                   (cons (list 'if t-body) t-body-tail)))))
          ((integer? (peek stream))
           (let* ((t-integer (next stream))
                  (t-body-tail (body stream error)))
             (cons t-integer t-body-tail)))
          ((and (symbol? (peek stream))
                (not (eqv? (peek stream) 'endif))
                (not (eqv? (peek stream) 'define))
                (not  (eqv? (peek stream) 'if))
                (not (eqv? (peek stream) 'end)))
           (let* ((t-word (next stream))
                  (t-body-tail (body stream error)))
             (cons t-word t-body-tail)))
          (else '())))
  (define stream (make-stream (vector->list tokens) "EOF"))
  (call-with-current-continuation
   (lambda (error)
     (define res (program stream error))
     (and (equal? (peek stream) "EOF")
          res))))


;============================================
;tests


(define parse-tests
  (list (test (parse #(1 2 +))
              (() (1 2 +)))
        (test (parse #(x dup 0 swap if drop -1 endif))
              (() (x dup 0 swap (if (drop -1)))))
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
                         4 factorial ))
              (((-- (1 -))
                (=0? (dup 0 =))
                (=1? (dup 1 =))
                (factorial
                 (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
               (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial)))
        (test (parse #(define word w1 w2 w3))
              #f)
        (test (parse #(0 if 1 if 2 endif 3 endif 4))
              (() (0 (if (1 (if (2)) 3)) 4)))
        (test (parse #(define =0? dup 0 = end
                        define gcd
                        =0? if drop exit endif
                        swap over mod
                        gcd
                        end
                        90 99 gcd
                        234 8100 gcd))
              (((=0? (dup 0 =))
                (gcd (=0?
                      (if (drop exit))
                      swap over mod
                      gcd)))
               (90 99 gcd
                   234 8100 gcd)))))

(run-tests parse-tests)
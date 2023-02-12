;=====================1
(define r #f)

(define-syntax use-assertions
  (syntax-rules ()
    ((use-assertions)
     (call-with-current-continuation
      (lambda (c)
        (set! r c))))))
(define-syntax assert
  (syntax-rules()
    ((assert expr)
     (if (eval expr (interaction-environment))
         "DONE"
         (begin
           (display "FAILED: ")
           (r (display 'expr)))))))

(use-assertions)
(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

;=====================2

(define (save-data x dir)
  (with-output-to-file dir
    (lambda ()
      (write x))))

(define (load-data dir)
  (with-input-from-file dir
    (lambda ()
      (display (read)))))


(define (count-line file)
  (call-with-input-file file
    (lambda (port)
      (define str1 "")
      (define str2 "")
      (define (read-loop count)
        (set! str1 str2)
        (set! str2 (read-char port))
        (if (eof-object? str2)
            count
            (if (or (and (eq? str2 #\return) (not (eq? str1 #\newline)))
                    (and (eq? str2 #\newline) (not (eq? str1 #\newline)) (not (eq? str1 #\return))))
                (read-loop (+ count 1))
                (read-loop count))))
      (read-loop 0))))

;=====================3

(define (trib n)
  (if (<= n 1)
      0
      (if (= n 2)
          1
          (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3))))))

(define memory '(((trib-memory 0) . 0)
                 ((trib-memory 1) . 0)
                 ((trib-memory 2) . 1)))

(define (trib-memory n)
  (or (letrec ((loop (lambda (m)
                       (and (not (null? m))
                            (if (equal? `(trib-memory ,n) (caar m))
                                (cdar m)
                                (loop (cdr m)))))))
        (loop memory))
      (let ((rez (+ (trib-memory (- n 1))
                    (trib-memory (- n 2))
                    (trib-memory (- n 3)))))
        (begin (set! memory (cons `((trib-memory ,n) . ,rez) memory))
               rez))))

;=====================4

(define-syntax my-if
  (syntax-rules ()
    ((my-if condition ift iff)
     (force (or (and condition
                     (delay ift))
                (delay iff))))))

;=====================5

(define-syntax my-let
  (syntax-rules ()
    ((my-let ((value mean)) action)
     ((lambda (value) action) mean))
    ((my-let ((value mean) . assig) action)
     (my-let assig ((lambda (value) action) mean)))))
(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((value mean)) action)
     ((lambda (value) action) mean))
    ((my-let* ((value mean) . assig) action)
     ((lambda (value) (my-let* assig action)) mean))))

;=====================6
;А
(define-syntax when
  (syntax-rules ()
    ((when condition action) (and condition action))                         
    ((when condition . actions) (and condition (begin . actions)))))

(define x 2)

(define-syntax unless
  (syntax-rules ()
    ((unless condition action) (if (not (quote condition)) action))                         
    ((unless condition . actions) (if (not condition) (begin . actions)))))

;Б
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . actions) (letrec ((loop (lambda (xs1)
                                              (if (not (null? xs1))
                                                  (let ((x (car xs1)))
                                                    (begin (begin . actions) (loop (cdr xs1))))))))
                               (loop xs)))
    ((for xs as x . actions) (for x in xs . actions))))

;B
(define-syntax while
  (syntax-rules ()
    ((while condition . actions)
     (letrec ((loop (lambda ()
                      (if (and condition)
                          (begin (begin . actions) (loop))))))
       (loop)))))

;Г
(define-syntax repeat
  (syntax-rules (until)
    ((repeat actions until condition) (letrec ((loop (lambda ()
                                                       (begin (begin . actions)
                                                              (if (not (and condition))
                                                                  (loop))))))
                                        (loop)))))

;Д
(define-syntax cout
  (syntax-rules (<< endl)
    ((cout) (display ""))
    ((cout endl . body)
     (begin
       (newline)
       (cout . body)))
    ((cout << . body)
     (cout . body))
    ((cout expr . body)
     (begin
       (display expr)
       (cout . body)))))

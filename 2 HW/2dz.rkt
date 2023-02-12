;================================'
1
(define (my-range a b d)
  (if (> a b)
      (list)
      (cons a (my-range (+ a d) b d))))

(my-range  0 11 3) ; (0 3 6 9)
;=================================
(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))

(my-flatten '((1) 2 (3 (4 5)) 6)) ; (1 2 3 4 5 6)
;=================================
(define (my-element? x xs)
  (if (null? xs)
      #f
      (or (equal? x (car xs)) (my-element? x (cdr xs)))))

(my-element? 1 '(3 2 1)) ; #t
(my-element? 4 '(3 2 1)) ; #f
;=================================
(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

(my-filter odd? (my-range 0 10 1))
; (1 3 5 7 9)
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
; (0 3 6 9 12)
;================================


(define (my-fold-right op xs)
  (if (<= (length xs) 2)
      (if (= (length xs) 2)
          (op (car xs) (car (cdr xs)))
          xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(define (my-fold-right op xs)
  (cond ((= (length xs) 1) (car xs))
        ((= (length xs) 2) (op (car xs) (cadr xs)))
        (else (op (car xs) (my-fold-right op (cdr xs))))))
;================================
(define (my-fold-left op xs)
  (if (= (length xs) 1)
      (car xs)
      (my-fold-left op (cons (op (car xs) (car(cdr xs))) (cdr(cdr xs))))))

;================================
(define (list->set xs)
  (if (null? xs)
      '()
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))))

(list->set '(1 1 2 3))                       ;(3 2 1)
;================================
(define (set? xs)
  (if (null? xs)
      (and)
      (if (my-element? (car xs) (cdr xs))
          (or)
          (and (set? (cdr xs))))))

(set? '(1 2 3))                            ;#t
(set? '(1 2 3 3))                          ;#f
(set? '())                                 ;#t
;================================
(define (union xs ys)
  (list->set (append xs ys)))

(union '(1 2 3) '(2 3 4))                  ; (4 3 2 1)
;================================
(define (intersection xs ys)
  (if (null? xs)
      '()
      (if (my-element? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))

(intersection '(1 2 3) '(2 3 4))            ; (2 3)
;================================
(define (difference xs ys)
  (if (null? xs)
      '()
      (if (my-element? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))))

(difference '(1 2 3 4 5) '(2 3))             ; (1 4 5)
;================================
(define (symmetric-difference xs ys)
  (if (null? xs)
      '()
      (difference (union xs ys) (intersection xs ys))))

(symmetric-difference '(1 2 3 4) '(3 4 5 6)) ; (6 5 2 1)
;================================
(define (set-eq? xs ys)
  (equal? (difference (union xs ys) (intersection xs ys)) '()))

(set-eq? '(1 2 3) '(3 2 1))                  ; #t
(set-eq? '(1 2) '(1 3))                      ; #f
;================================
;================================
(define (string-trim-left xs)
  (if (char-whitespace? (string-ref xs 0))
      (string-trim-left (substring xs 1))
      xs))

(string-trim-left  "\t\tabc def")   ; "abc def"
;================================
(define(string-trim-right xs)
  (if (char-whitespace? (string-ref xs (-(string-length xs) 1)))
      (string-trim-right (substring xs 0 (-(string-length xs) 1)))
      xs))

(string-trim-right "abc def\t")     ; "abc def"
;================================
(define (string-trim xs)
  (string-trim-left(string-trim-right xs)))

(string-trim       "\t abc def \n") ; "abc def"
;================================
(define (string-prefix? a b)
  (if (>(string-length a) (string-length b))
      (or)
      (if (=(string-length a) (string-length b))
          (equal? a b)
          (string-prefix? a (substring b 0 (string-length a))))))

(string-prefix? "abc" "abcdef")  ; #t
(string-prefix? "bcd" "abcdef")  ; #f
(string-prefix? "abcdef" "abc")  ; #f
;================================
(define (string-suffix? a b)
  (if (>(string-length a) (string-length b))
      (or)
      (if (=(string-length a) (string-length b))
          (equal? a b)
          (string-suffix? a
                          (substring b (-(string-length b) (string-length a)))))))

(string-suffix? "def" "abcdef")  ; #t
(string-suffix? "bcd" "abcdef")  ; #f
(newline)
;================================
(define (string-infix? a b)
  (if (>(string-length a) (string-length b))
      (or)
      (if (string-prefix? a b)
          (and)
          (string-infix? a (substring b 1)))))

(string-infix? "def" "abcdefgh") ; #t
(string-infix? "abc" "abcdefgh") ; #t
(string-infix? "fgh" "abcdefgh") ; #t
(string-infix? "ijk" "abcdefgh") ; #f
(string-infix? "bcd" "abc")      ; #f
;=================================
(define (string-split str sep)
  (if (null? str)
      (list)
      (if (string-prefix? sep str)
          (string-split (substring str (- (string-length str) (string-length sep))))
          (begin
            (display (substring str 0 1))
            (string-split (substring str (- (string-length str) (string-length sep))) sep))))) 
  



;(string-split "x;y;z" ";")       ⇒ ("x" "y" "z")
;(string-split "x-->y-->z" "-->") ⇒ ("x" "y" "z")


(define (string-trim-left xs)
  (if (char-whitespace? (string-ref xs 0))
      (string-trim-left (substring xs 1))
      xs))

(define(string-trim-right xs)
  (if (char-whitespace? (string-ref xs (-(string-length xs) 1)))
      (string-trim-right (substring xs 0 (-(string-length xs) 1)))
      xs))

(define (string-trim xs)
  (string-trim-left(string-trim-right xs)))

(define (string-prefix? a b)
  (if (>(string-length a) (string-length b))
      (or)
      (if (=(string-length a) (string-length b))
          (equal? a b)
          (string-prefix? a (substring b 0 (string-length a))))))

(define (string-suffix? a b)
  (if (>(string-length a) (string-length b))
      (or)
      (if (=(string-length a) (string-length b))
          (equal? a b)
          (string-suffix? a
                          (substring b (-(string-length b) (string-length a)))))))

(define (string-infix? a b)
  (if (>(string-length a) (string-length b))
      (or)
      (if (string-prefix? a b)
          (and)
          (string-infix? a (substring b 1)))))

(define (string-split str sep)
  (if (= (string-length str) 0 )
      (list)
      (if (string-prefix? sep str)
          (string-split (substring str (string-length sep) (string-length str)) sep)
          (begin
            (cons(substring str 0 1)
            (string-split (substring str 1 (string-length str)) sep)))))) 
; Exercise 1.3

(define (square x) (* x x)) 
  
(define (sumsquares x y) (+ (square x) (square y))) 

(define (sqsumlargest a b c) 
    (cond  
        ((and (>= a c) (>= b c)) (sumsquares a b)) 
        ((and (>= b a) (>= c a)) (sumsquares b c)) 
        (else (sumsquares a c))))


; Exercise 1.7

(define (sqrt-iter old-guess new-guess x) (if (good-enough? old-guess new-guess)
      new-guess
      (sqrt-iter new-guess (improve new-guess x) x)))

(define (improve guess x) (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? old-guess new-guess)
(< (/ (abs (- old-guess new-guess)) old-guess) 0.001))

(define (sqrt x) (sqrt-iter 0.0 1.0 x))

; Exrecise 1.8

(define (curt-iter old-guess new-guess x)
    (if (good-enough? old-guess new-guess)
    new-guess
    (curt-iter new-guess (curt-improve new-guess x) x)))

(define (curt-improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (curt x) (curt-iter 0.0 1.0 x))


; Exerise 1.9


; (define (+ a b)
;     (if (= a 0) b (inc (+ (dec a) b))))
; ; 4 and 5
;     (if (= 4 0) 5 (inc (+ 3 5)))
;     (if (= 4 0) 5 (inc (if (= 3 0) 5 (inc (+ 2 5)))))
; ;recursive


; (define (+ a b)
;     (if (= a 0) b (+ (dec a) (inc b))))
; ; 4 and 5
;     (if (= 4 0) 5 (+ 3 6))
;     (if (= 4 0) 5 (if (= 3 0) 6 (+ 2 7)))
;iterative

; 1.10

(define (A x y) (
    cond ((= y 0) 0)
         ((= x 0) (* 2 y))
         ((= y 1) 2)
         (else (A (- x 1) (A x (- y 1))))))

; (define (f n) (A 0 n))
;  -> 2n

;  (define (g n) (A 1 n))
;   ->(A 0 (A 1 (- n 1))
;   ->2 * (A 1 (- n 1)

; Exercise 1.11

(define (rfunc n) (
    cond ((< n 3) n)
         (else (+ (rfunc (- n 1))
               (* 2 (rfunc (- n 2)))
               (* 3 (rfunc (- n 3)))
         ))))

(define (total a b c) (+ a (* 2 b) (* 3 c)))
(define (ifunc-iter n i a b c) (
    cond ((< n 3) n)
         ((> i n) (total a b c))
         (else (ifunc-iter n (+ 1 i) (total a b c) a b))))

(define (ifunc n) (ifunc-iter n 4 2 1 0))

; Wiki solution, counts down from n instead of up from 0
 (define (f n) 
   (define (iter a b c count) 
     (if (= count 0) 
       a 
       (iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))) 
   (iter 0 1 2 n)) 

; Exercise 1.12
(define (pascal n x) (
    cond ((= x 1) 1)
         ((= x n) 1)
         ((> x n) 0)
         (else (+ (pascal (- n 1) x) (pascal (- n 1) (- x 1))))
))
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

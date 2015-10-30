#lang racket

(define (f p g h)
  (lambda (x) (and (p (g x)) (p (h x)))))

(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (add-frac frac1 frac2)
  (cond
    [(= (snd frac1) (snd frac2)) (cons (+ (fst frac1) (fst frac2)) (snd frac1))]
    [else (simplify-frac (cons (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))]))

(define (subtract-frac frac1 frac2)
  (cond
    [(= (snd frac1) (snd frac2)) (cons (- (fst frac1) (fst frac2)) (snd frac1))]
    [else (simplify-frac (cons (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))]))

(define (mult-frac frac1 frac2)
  (simplify-frac (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2)))))

(define (gcd a b)
  (define (helper1 a b)
    (if (> a b)
        (helper2 a b)
        (helper2 b a)))
  (define (helper2 a b)
    (if (= b 0)
        a
        (helper2 b (remainder a b))))
  (cond
    [(= a 0) (abs b)]
    [(= b 0) (abs a)]
    [else (helper1 (abs a) (abs b))]))

(define (simplify-frac frac)
  (cons (/ (fst frac) (gcd (fst frac) (snd frac))) (/ (snd frac) (gcd (fst frac) (snd frac)))))

#lang racket
(define (half-perimeter a b c)
  (/ (+ a b c) 2))

(define (area a b c)
  (if (and (> a 0) (> b 0) (> c 0))
      (sqrt (* (half-perimeter a b c) (- (half-perimeter a b c) a) (- (half-perimeter a b c) b) (- (half-perimeter a b c) c ) ))
      "Not valid input!"))

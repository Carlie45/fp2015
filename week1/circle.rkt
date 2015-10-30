#lang racket
(define (square x)
  (* x x))

(define (circle? point-x point-y circle-x circle-y radius)
  (if (<= (+ (square (- point-x circle-x)) (square (- point-y circle-y))) (square radius))
      "The point is in the circle"
      "The point is NOT in the circle"))

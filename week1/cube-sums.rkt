#lang racket

(define (cube x)
  (* x x x))

(define (cube-sums? n a b)
  (if (= (+ (cube a) (cube b)) n)
      #t
      (if (< (+ (cube a) (cube b)) n)
          (if (<= a b)
              (cube-sums? n (+ a 1) b)
              (cube-sums? n a (+ b 1)))
          #f)))

(define (count-cube-sums count from to)
  (if (> from to)
      "Incorrect input!"
      (if (< from to)
          (if (cube-sums? from 1 1)
              (count-cube-sums (+ count 1) (+ from 1) to)
              (count-cube-sums count (+ from 1) to))
          count)))

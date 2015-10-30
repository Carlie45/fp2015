#lang racket
(require racket/block)

(define (number-of-divisors n divisor count)
  (if (= n divisor)
      (+ count 1)
      (if (= (remainder n divisor) 0)
          (number-of-divisors n (+ divisor 1) (+ count 1))
          (number-of-divisors n (+ divisor 1) count))))

(define (prime? n)
  (if (<= n 0)
      "Bad input!"
      (if (> (number-of-divisors n 1 0) 2)
          "The number is NOT prime!"
          "The number is prime")))

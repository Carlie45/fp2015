#lang racket
(define (product-of-digits n)
  (if (> n 0)
      (if (< n 10)
          n
          (* (remainder n 10) (product-of-digits (/(- n (remainder n 10)) 10))))
      "not a positive number"))
  

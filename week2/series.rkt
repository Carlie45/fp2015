#lang racket
(define (series a b n)
  (define (series-iter a b n result i)
    (if (= n 1)
      a
      (if (= n 2)
          b                       
          (if (> n i)
                (series-iter b (+ a b) n (+ result a) (+ i 1))
                result))))
            (series-iter a b n b 2))

(define (lucas n)
  (series 2 1 n))

(define (fibonacci n)
  (series 1 1 n))

(define (summed-member n)
  (define (summed-member-iter n i result)
    (if (= n 1)
        (+ (lucas 1) (fibonacci 1))
        (if (> n i)
            (summed-member-iter n (+ i 1) (+ result (lucas (- i 1)) (fibonacci (- i 1))))
            result)))
  (summed-member-iter n 1 0))

(define (nth-lucas-sum n)
  (define (nth-lucas-sum-iter n i result)
    (if (= n 1)
        (lucas 1)
        (if (>= n i)
            (nth-lucas-sum-iter n (+ i 1) (+ result (lucas i)))
            result)))
  (nth-lucas-sum-iter n 1 0))

(define (nth-fibonacci-sum n)
  (define (nth-fibonacci-sum-iter n i result)
    (if (= n 1)
        (fibonacci 1)
        (if (>= n i)
            (nth-fibonacci-sum-iter n (+ i 1) (+ result (fibonacci i)))
            result)))
  (nth-fibonacci-sum-iter n 1 0))

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))

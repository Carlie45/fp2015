#lang racket

(provide string-reverse
         to-binary-string)

(define (nth-beast-number n)
  (define (nth-beast-number-iter str n)
    (if (= n 1)
        (string-append str "666")
        (nth-beast-number-iter (string-append str "666") (- n 1))))
  (nth-beast-number-iter "" n))

(define (reverse-int n)
  (define (rev-iter n result)
    (cond
      [(= n 0) result]
      [else (rev-iter (quotient n 10) (+ (* result 10) (remainder n 10)))]))
  (rev-iter n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (count-1 n)
  (define (count-1-iter n count)
    (cond
      [(= n 0) count]
      [(= n 1) (+ count 1)]
      [(= (remainder n 10) 1) (count-1-iter (quotient n 10) (+ count 1))]
      [else (count-1-iter (quotient n 10) count)]))
  (count-1-iter (string->number (to-binary-string n)) 0))

(define (is-hack-number n)
  (and (palindrome? (string->number (to-binary-string n))) (odd? (count-1 n))))

(define (next-hack n)
   (cond
     [(is-hack-number (+ n 1)) (+ n 1)]
     [else (next-hack (+ n 1))]))

(define (p-score n)
  (define (p-score-iter n count)
    (cond
      [(palindrome? n) (+ count 1)]
      [else (p-score-iter (+ n (reverse-int n)) (+ count 1))]))
  (p-score-iter n 0))

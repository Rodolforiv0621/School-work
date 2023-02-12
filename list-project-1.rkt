#lang racket
(define (rotate-left-1 x)
  (cond
    ((empty? x) x)
    (else (append (cdr x) (list (car x))))
 ))

(define (rotate-left-n x n)
  (cond
    ((eq? 0 n) x)
    (else (rotate-left-n (append (cdr x) (list (car x))) (- n 1)))
  ))

(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items(cdr x))))
    )
  )
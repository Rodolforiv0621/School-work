#lang racket
(define (rotate-left-1 x)
  (cond
    ((empty? x) x)
    (else (append (cdr x) (list (car x))))
 ))


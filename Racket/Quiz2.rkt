#lang racket
(define (list-last list)
  (cond
    ((empty? (cdr list)) car list)
    (else (list-last (cdr list)))
    ))

(define (list-numbers? list)
  (cond
    ((empty? list) #t)
    ((not (number? (car list))) #f)
    (else (list-numbers? (cdr list)))))

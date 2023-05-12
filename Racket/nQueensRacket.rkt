#lang racket
(define(nc-1 a b d)
  (and (not (= a b))
       (not (= d (abs(- a b))))))

(define (nc v p)
  (nc-h v p 1))

(define (nc-h v p d)
  (cond
  ((empty? p) #t)
  (else (and (nc-1 v (car p) d) (nc-h v (cdr p) (+ d 1))))
    ))

(define (solve n)
  (solve-h n 1 '(0) '(0 1) '()))

(define (solve-h n c qp nr sv)
  (cond
    ((and (= (car nr) n) (empty? qp)) sv)
    ((= c n) (solve-h n (- c 1) (cdr qp) (cdr nr) (cons qp sv)))
    ((nc (car nr) qp) (solve-h n (+ c 1) (cons (car nr) qp) (cons 0 nr) sv))
    ((= (car nr) n) (solve-h n (- c 1) (cdr qp) (cdr nr) sv))
    (else (solve-h n c qp (cons (+ 1 (car nr)) (cdr nr)) sv))))

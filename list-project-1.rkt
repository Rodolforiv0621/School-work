#lang racket
; rotates list left by 1, wraps around
; param {x}: list
(define (rotate-left-1 x)
  (cond
    ((empty? x) x)
    (else (append (cdr x) (list (car x))))
 ))

; rotate left n amount of times, wraps around
; param {x}: list
; param {n}: n amount of times
(define (rotate-left-n x n)
  (cond
    ((eq? 0 n) x)
    (else (rotate-left-n (append (cdr x) (list (car x))) (- n 1)))
  ))

; counts amount of items in list
; param {x}: list
(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items(cdr x))))
    )
  )

; finds the nth element is list
; param {x}: list
; param {n}: number to be found
(define (list-item-n x n)
  (cond
    ((eq? 0 n) (car x))
    (else (list-item-n (cdr x) (- n 1)))
  ))

; deletes nth item from list
; param {x}: list
; param {n}: number to be deleted
(define (list-minus-item-n x n)
  (cond
    ((eq? 0 n) (cdr x))
    (else (cons (car x) (list-minus-item-n (cdr x) (- n 1))))
  ))

; rotates list by one to the rights, wraps around
; param {x}: list
(define (rotate-right-1 x)
  (cond
    ((empty?  x) x) ; base case: if empty return x
    (else (cons (list-item-n x ( - (count-items x) 1)) (list-minus-item-n x (- (count-items x) 1))))
  ))

; lists items in reverse order
; param {x}: list
(define (reverse-list x)
  (cond
    ((empty? (cdr x)) x)
    (else (append (reverse-list (cdr x)) (list (car x))))
  ))

; uses cons to add one element to multiple lists in a list
; param {a}: element
; param {x}: list of lists
(define (cons-to-all a x)
  (cond
    ((empty? (cdr x)) (list (cons a (car x))))
    (else (append (list (cons a (car x))) (cons-to-all a (cdr x))))))

; generates all permutations of original list
; param {x}: list
(define (permute x)
  (cond
    ((empty? x) null)
    ((empty? (cdr x)) (list x))
    ((empty? (cddr x)) (list x (reverse x)))
    (else (ph-2 x (-(length  x) 1)))
    ))

; permute helper fucntion 1
; param {x}: list
; param {n}: element
(define (ph-1 x n)
  (cons-to-all (list-item-n x n) (permute (list-minus-item-n x n))))

; permute helper function 2
; param {x}: list
; param {n}: element
(define (ph-2 x n)
  (cond
    ((eq? n 0) (ph-1 x 0))
    (else (append (ph-1 x n)(ph-2 x (- n 1))))))


(rotate-left-1 '())   ; '() 
(rotate-left-1 '(a))   ; '(a) 
(rotate-left-1 '(a b c))   ; '(b c a) 
(rotate-left-n '(a b c) 0)    ; '(a b c) 
(rotate-left-n '(a b c d e) 2)  ; '(c d e a b) 
(rotate-left-n '(a b c d e) 5)  ; '(a b c d e) 
(count-items '())   ; 0 
(count-items '(a))   ; 1 
(count-items '(a b c d e))  ; 5 
(list-item-n '(a b c d e) 0)  ; 'a 
(list-item-n '(a b c d e) 4)  ; 'e 
(list-item-n '(a b c d e) 1)  ; 'b 
(list-minus-item-n '(a b c d e) 0)   ; (b c d e) 
(list-minus-item-n '(a b c d e) 1)   ; '(a c d e) 
(list-minus-item-n '(a b c d e) 2)   ; '(a b d e) 
(list-minus-item-n '(a b c d e) 4)   ; '(a b c d) 
(rotate-right-1 '(a b c d e))  ; '(e a b c d) 
(rotate-right-1 '(a))   ; '(a) 
(rotate-right-1 '(a b))   ; '(b a) 
(rotate-right-1 '(a b c d e f g))  ; '(g a b c d e f) 
(reverse-list '(a))   ; '(a) 
(reverse-list '(a b))   ; '(b a) 
(reverse-list '(a b c d e))   ; '(e d c b a) 
(cons-to-all 'a '((b c) (d e) (f g)))  ; '((a b c) (a d e) (a f g)) 
 
(permute '(a b))    ; '((a b) (b a)) 2! = 2 permutations 
(permute '(a b c))   ; '((c a b) (c b a) (b a c) (b c a) (a b c) (a c b)) 3! = 6 permutations 
(permute '(a b c d)) 
 
; '((d c a b)  (d c b a)  (d b a c)  (d b c a)  (d a b c)  4! = 24 permutations 
;  (d a c b)  (c d a b)  (c d b a)  (c b a d)  (c b d a) 
;  (c a b d)  (c a d b)  (b d a c)  (b d c a)  (b c a d) 
;  (b c d a)  (b a c d)  (b a d c)  (a d b c)  (a d c b) 
;  (a c b d)  (a c d b)  (a b c d)  (a b d c)) 
 
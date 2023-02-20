#lang racket
;Rodolfo Rivera
;Comp 333
;2/17/23

; rotates list left by 1, wraps around
; param {x}: list
(define (rotate-left-1 x)
  (cond
    ((empty? x) x) ;base case: if x is empty no operation is needed return
    (else (append (cdr x) (list (car x)))) ; add first elent to end of rest of list
 ))

; rotate left n amount of times, wraps around
; param {x}: list
; param {n}: n amount of times
(define (rotate-left-n x n)
  (cond
    ((eq? 0 n) x) ; base case: stop when n is 0 and return
    (else (rotate-left-n (append (cdr x) (list (car x))) (- n 1))) ; recursion stop, add rest of list to first element
  ))

; counts amount of items in list
; param {x}: list
(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items(cdr x)))) ; cdr list until it is empty
    )
  )

; finds the nth element is list
; param {x}: list
; param {n}: number to be found
(define (list-item-n x n)
  (cond
    ((eq? 0 n) (car x)) ; when n = 0 return car x
    (else (list-item-n (cdr x) (- n 1))) ; cdr list until you find item and
  ))

; deletes nth item from list
; param {x}: list
; param {n}: number to be deleted
(define (list-minus-item-n x n)
  (cond
    ((eq? 0 n) (cdr x)); when n = 0, skip target number
    (else (cons (car x) (list-minus-item-n (cdr x) (- n 1)))); adds car of x to recursive call
  ))

; rotates list by one to the rights, wraps around
; param {x}: list
(define (rotate-right-1 x)
  (cond
    ((empty?  x) x) ; base case: if empty return x
    (else (cons (list-item-n x ( - (count-items x) 1)) (list-minus-item-n x (- (count-items x) 1)))); cons target element to rest of list without target item
  ))

; lists items in reverse order
; param {x}: list
(define (reverse-list x)
  (cond
    ((empty? (cdr x)) x); when only one element return
    (else (append (reverse-list (cdr x)) (list (car x)))); appends the reversed list with first item
  ))

; uses cons to add one element to multiple lists in a list
; param {a}: element
; param {x}: list of lists
(define (cons-to-all a x)
  (cond
    ((empty? (cdr x)) (list (cons a (car x)))); if list 1 element left, return cons of element and first list
    (else (append (list (cons a (car x))) (cons-to-all a (cdr x)))))); recursive step, appends element to first element and calls itself 

; generates all permutations of original list
; param {x}: list
(define (permute x)
  (cond
    ((empty? x) null) 
    ((empty? (cdr x)) (list x)) ;if only one element left return element as list
    ((empty? (cddr x)) (list x (reverse x)));if 2 elements left return list with the reverse of list
    (else (ph-2 x (-(length  x) 1))) ; call helper fucntion
    ))

; permute helper fucntion 1
; param {x}: list
; param {n}: element
(define (ph-1 x n)
  (cons-to-all (list-item-n x n) (permute (list-minus-item-n x n)))); uses con to all function to add element to permutation

; permute helper function 2
; param {x}: list
; param {n}: element
(define (ph-2 x n)
  (cond
    ((eq? n 0) (ph-1 x 0)) ; base case; if n = 0, call helper function 1
    (else (append (ph-1 x n)(ph-2 x (- n 1)))))); appends permutations together


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
 
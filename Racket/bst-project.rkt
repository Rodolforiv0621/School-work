#lang racket
;Rodolfo Rivera
;3/17/23
;bst project

(provide insert)
;(provide delete)
(provide traverse)
(provide find)
(provide nl-to-vl)
(provide bst)
(provide random-list)
(provide insert-from-list)
(provide nl-to-vcl)
(provide node-value)
(provide get-iop-path)
(provide node?)
(provide node)
(provide nl-to-vl)
(provide node-count)
(provide bst?)
(provide bst-root)


;------------------------
; bst and node struct 
;------------------------
(struct bst (root) #:mutable #:transparent)
(struct node (value count left right) #:mutable #:transparent)

;--------------------------------
; Random number functions
;--------------------------------
(define (random-value r)
  (inexact->exact (floor (* (random) r)))
  )

(define (random-list n r)
  (cond
    ((= n 1) (list (random-value r)))
    (else (cons (random-value r) (random-list (- n 1) r)))))

;(define b1 (bst null))

;--------------------------------
; Turns node list to value list
;-------------------------------- 
(define (nl-to-vl x)
  (cond
    ((empty? x) '())
    (else (cons (node-value (car x)) (nl-to-vl (cdr x))))
    ))

;--------------------------------------------
; Returns a path of nodes from root to node v
;--------------------------------------------
(define (find t v)
  (cond
    ((empty? (bst-root t)) null)
    (else (find-node (bst-root t) v null))
    ))

;---------------------------------------------------
; Returns list of nodes from root to insertion point 
;---------------------------------------------------
(define (find-node n v stack)
  (cond
    ((empty? n) stack)
    ((= v (node-value n)) (cons n stack))
    ((< v (node-value n)) (find-node (node-left n) v (cons n stack)))
    (else (find-node (node-right n) v (cons n stack)))
    ))

;--------------------------------
; In-order traversal of bst
;--------------------------------
(define (traverse t)
  (traverse-node (bst-root t)))

;-----------------------------------------
; Returns list of bst to traverse function
;-----------------------------------------
(define (traverse-node n)
  (cond
    ((empty? n) '())
    (else (append (traverse-node (node-left n)) (list n) (traverse-node (node-right n))))))

;--------------------------------
; Inserts value v into bst t
;--------------------------------
(define (insert t v)
  (cond
    ((empty? (bst-root t)) (set-bst-root! t (node v 1 '() '())))
    (else (insert-node v (find t v))
    )))

;---------------------------------------------
; Insert value v into path based on conditions
;---------------------------------------------
(define (insert-node v path)
  (cond
    ((= v (node-value (car path))) (set-node-count! (car path) (+ 1 (node-count (car path)))))
    ((< v (node-value (car path))) (set-node-left! (car path) (node v 1 '() '() )))
  (else (set-node-right! (car path) (node v 1 '() '() )))))

;---------------------------------
; Displays bst as a list of values
;---------------------------------
(define (insert-from-list t y)
(map (lambda (x) (insert t x) (displayln (nl-to-vl (traverse t)))) y) (void)
)

;-----------------------------
; Finds path from n to its iop
;-----------------------------
(define (get-iop-path n)
  (get-iop-path-right (node-left n) (list n)))

(define (get-iop-path-right n path)
  (cond
    ((empty? (node-right n)) (cons n path))
    (else (get-iop-path-right (node-right n) (cons n path)))))

;---------------------------------------------
; Insert value v into path based on conditions
;---------------------------------------------

;(define (delete-node path)
;  (let ((n (car path)))
;    (cond
 ;     ((= 0 (child-count n)) (delete-node-0 path))
;      ((= 1 (child-count n)) (delete-node-1 path))
 ;     (else (delete-node-2 path))
 ;     )))

;(define (delete t v)
;  (let ((path (find t v)))
;    (cond
;      ((empty? t) )
;      ((empty? (bst-root t)) )
 ;;     ((not (= v (node-value(car path)))))
;      ((and (= v (node-value (car path)))
;            (> (node-count (car path)) 1))
 ;      (set-node-count! (bst-root t))
 ;     ()
 ;     (else (delete-node path))))))

;-------------------------------
; Counts number of children of n
;-------------------------------
(define (child-count n)
  (cond
    ((and (empty? (node-left n)) (empty? (node-right n))) 0)
    ((and (not (empty? (node-left n))) (not (empty? (node-right n)))) 2)
    (else 1)))

(define (delete-node-0 path)
  (let ((n (car path))) 
  (cond
    ((empty? path) )
    ((= n (node-left (car path))) (set-node-left! (car path) '()))
    (else (set-node-right! (car path) '())))))

  
;(define (delete-node-1 path)
  

;-----------------------------------------
; gets child node values and retur-ns list
;-----------------------------------------                 
  (define (get-child-node-values n)
(cond
((and(empty? (node-left n))(empty? (node-right n)))
(list 'X 'X))
((and(empty? (node-left n))(not(empty? (node-right n))))
(list 'X (node-value (node-right n))))
((and(not(empty? (node-left n)))(empty? (node-right n)))
(list (node-value (node-left n)) 'X))
(else
(list (node-value(node-left n))(node-value(node-right n))))
))

;-------------------------------
; Counts number of children of n
;-------------------------------
  (define (get-node-fields n)
(cond
((empty? n) "X")
(else (append (list (node-value n) (node-count n)) (get-child-node-values n)))
))

  (define (nl-to-all x)
(cond
((empty? x) (list 'X))
(else (append (list (get-node-fields (car x))) (nl-to-all (cdr x))))
))

;-------------------------------
; Counts number of children of n
;-------------------------------
(define (nl-to-vcl x)
  (cond
    ((= 1 (length x)) (cons(list (node-value (car x)) (node-count (car x))) (cdr x)))
    (else (cons(list(node-value (car x)) (node-count (car x))) (nl-to-vcl(cdr x))))))
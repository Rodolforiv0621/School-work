#lang racket
;Rodolfo Rivera
(require "bst-project.rkt")

;---------------------------------------------------
; Test case part 1
;---------------------------------------------------
(define n10 (node 70 1 null null))
(define n09 (node 65 1 null n10))
(define n08 (node 30 1 null null))
(define n07 (node 87 1 null null))
(define n06 (node 60 1 null n09))
(define n05 (node 37 1 n08 null))
(define n04 (node 12 1 null null))
(define n03 (node 75 1 n06 n07))
(define n02 (node 25 1 n04 n05))
(define n01 (node 50 1 n02 n03))

(define b01 (bst n01))

(node? n10) ; test for membership for node n10
(node-value n10) ; access of value field of node n10
(node-count n08) ; access of count field of node n08
(bst? b01) ; test for bst membership for b01
(empty? (bst-root b01)) ; check if root of b01 is empty

(find b01 70)
(find b01 50)
(nl-to-vl (find b01 70))
(nl-to-vl (find b01 50))
(traverse b01)
(nl-to-vl (traverse b01))
(nl-to-vl (traverse b01))

(map (lambda (x) (find b01 x)) (nl-to-vl (traverse b01)))
(map (lambda (x) (nl-to-vl (find b01 x))) (nl-to-vl (traverse b01)))
;---------------------------------------------------
; Test case part 2.1
;---------------------------------------------------
(define btree (bst null)) ; create new bst named btree initialized to empty
 btree ; display its contents
(define rlist (random-list 15 1000)) ; create a random list rlist
 rlist ; display its contents
(insert-from-list btree rlist) ; insert all values from rlist into btree

;---------------------------------------------------
; Test case part 2.2
;---------------------------------------------------
(define btree-2 (bst null))
 btree-2
(define list-2 '(50 25 75 12 37 30 45 60 80 12 60))
 list-2
 (insert-from-list btree-2 list-2)
(nl-to-vcl (traverse btree-2))

;---------------------------------------------------
; Test case part 3.1
;---------------------------------------------------
(define q (bst null)) ; build tree
(define qlist '(50 25 75 70 80 72 71 74 73))
(insert-from-list q qlist)
(define n75 (car (find q 75))) ; find node 75
(node-value n75)
(nl-to-vl (get-iop-path n75)) ; find path from 75 to iop
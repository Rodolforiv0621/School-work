#lang racket

;------------------------
; bst and node struct 
;------------------------
(struct bst (root) #:mutable #:transparent)
(struct node (value count left right) #:mutable #:transparent)

;------------------------
;manually constructed tree for testing
;------------------------
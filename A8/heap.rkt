;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname heap) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.

;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 08, Problem 4
;; ***************************************************
(require "heap-support.rkt")


(define my-heap (make-hnode 1
                            (make-hnode 4
                                        (make-hnode 9 '() '()) 
                                        '())
                            (make-hnode 6
                                        (make-hnode 11 '() '())
                                        '())))

(heap-print my-heap number->string)
                            
;; Part A
;; heap-add takes in a comparison operator, an element, and a heap. Then insert the
;; element into the heap basedon the comparison operator.

;; Examples:
(check-expect (heap-add 0 empty <=) (make-hnode 0 empty empty))
(check-expect (heap-add 2 my-heap <=) (make-hnode
                                       1
                                       (make-hnode
                                        2
                                        (make-hnode 6 '() '())
                                        (make-hnode 11 '() '()))
                                       (make-hnode
                                        4
                                        (make-hnode 9 '() '())
                                        '())))

;; heap-add: (heapof X) X -> (heapof X)

(define (heap-add el heap comp?)
  (cond
    [(empty? heap) (make-hnode el empty empty)]
    [(comp? el (hnode-key heap))
     (make-hnode el
                (heap-add (hnode-key heap) (hnode-right heap) el)
                (hnode-left heap))] ;; add the key
    [else (make-hnode (hnode-key heap)
                      (heap-add el (hnode-right heap) comp?)
                      (hnode-left heap))]))

;; Tests: you can write these later


;; Part B
;; heap-remove-min takes in a heap and returns the same heap but
;; with its smallest item removed

;; Examples:
(check-expect (heap-remove-min my-heap <=)
              (make-hnode
               4
               (make-hnode 9 '() '())
               (make-hnode
                6
                (make-hnode 11 '() '())
                '())))

;; heap-remove-min: (heapof X) -> (heapof X)

(define (heap-remove-min heap comp?)
  (cond
    [ (or (empty? heap)
          (and (empty? (hnode-right heap)) (empty? (hnode-left heap)))) empty] 
    [(empty? (hnode-left heap)) (hnode-right heap)]
    [(empty? (hnode-right heap)) (hnode-left heap)]
    [(comp? (hnode-key (hnode-left heap)) (hnode-key(hnode-right heap)))
        (make-hnode (hnode-key (hnode-left heap))
                    (heap-remove-min (hnode-left heap) comp?)
                    (hnode-right heap))]
[else (make-hnode (hnode-key (hnode-left heap))
                  (hnode-left heap)
                  (heap-remove-min (hnode-right heap) comp?))]))

;; Tests:


;; Part C
;; list->heap turns a list into a heap through the recursive use of heap-add

;; Examples:
(check-expect (list->heap (list 1 4 6 7 11) <=) my-heap)

;;list->heap: (listof X) X<=? -> (heapof X)

(define (list->heap lst comp?)
  (cond
    [(empty? lst) empty]
    [else (heap-add (first lst) (list->heap (rest lst) comp?) comp?)]))

;; Tests:



;; Part D (CHECK IF IT GIVES RIGHT RESULT)
;; heap->list turns a heap into a list in ascending order, with the help of heap-remove-min 

;; Examples:
(check-expect (heap->list my-heap <=) (list 1 4 6 9 11))

(define (heap->list heap comp?)
  (cond
    [(empty? heap) empty]
    [else (cons (hnode-key heap) (heap->list (heap-remove-min heap comp?) comp?))]))
     
                                 
;; heap->list: (heapof X) X<=? -> (listof X)
;; Requires:


;; Part E
;; Examples:
;;(check-expect (heap-sort (list 4 5 1 3 5) <=) (list 1 3 4 5 5))
;;(check-expect (heap-sort (list -4 -1 -2 -3) <=) (list -4 -3 -2 -1))
;;(check-expect (heap-sort (list -4 -9 2 0) <=) (list -9 -4 0 2))
;;(check-expect (heap-sort empty <=) empty)

(define (heap-sort lst comp?)
  (cond
    [(empty? lst) empty]
  [else (heap->list (list->heap lst comp?) comp?)]))


  





              
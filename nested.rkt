;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 08, Problem 2
;; ***************************************************

;; Part A
;; nested-list-X-template: (nested-listof X) ->
;;(define (nested-list-X-template nestlist)
  
  

;; Part B
;; nested-count takes in a (nested-listof NotAList) and outputs how many NotAList
;; items there are

;; Examples:
(check-expect (nested-count '(1 2 "string")) 3)
(check-expect (nested-count '(a 3 ((59)) (((string))))) 4)
(check-expect (nested-count '((((1 2 3))) ((4) 5))) 5)

;; nested-count: (nested-listof NotAList) -> Nat

(define (nested-count lst)
  (cond
    [(empty? lst) 0]
    [(list? (first lst)) (+ (nested-count (first lst))
                         (nested-count (rest lst)))]
    [else (+ 1 (nested-count (rest lst)))]))

;; Tests:
(check-expect (nested-count '((((a43)) (i)) 3 () (()) (((baii)) (hi)))) 5)
(check-expect (nested-count '()) 0)
(check-expect (nested-count '(()((())) (()))) 0)
         
;; Part C
;; nested-sum takes in a (nested-listof Num) and outputs the sum of the Num values
;; within the nested list

;; Examples:
(check-expect (nested-sum '(1 1 2 3 5)) 12)
(check-expect (nested-sum '((0 -2) (2))) 0)
(check-expect (nested-sum '(-9 () 4)) -5)

;;nested-sum: (nested-lisof Num) -> Num

(define (nested-sum lst)
  (cond
   [(empty? lst) 0]
   [(list? (first lst)) (+ (nested-sum (first lst))
                           (nested-sum (rest lst)))]
   [else (+ (first lst) (nested-sum (rest lst)))]))

;; Tests:
(check-expect (nested-sum '(() (()))) 0)
(check-expect (nested-sum '()) 0)
(check-expect (nested-sum '(-1 ((-5.9) (0 ())))) -6.9)
            

;; Part D
;; nested-member? checks if any NotAList value in a nested list contains
;; a specific value

;; Examples:
(check-expect (nested-member? 'dog '((do) (g) (((dogg))))) false)
(check-expect (nested-member? 'dog '((dog) (g) (((dogg))))) true)
(check-expect (nested-member? "dog" '((do) (g) (((dogg))))) false)

;;nested-member?: Any (NotAList) -> Bool

(define (nested-member? val lst)
  (cond
   [(empty? lst) false]
   [(cons? (first lst)) (or (nested-member? val (first lst))
                           (nested-member? val (rest lst)))]
   [else (or (equal? (first lst) val) (nested-member? val (rest lst)))])) 

;; Tests:
(check-expect (nested-member? 0 '((do) (g) (((8))))) false)
(check-expect (nested-member? empty '(((() () (()))))) true)
(check-expect (nested-member? 8 '(((() (()))))) false)

             

;; Part E
;; Examples:
(check-expect (nested-ref '((0 1 2) (3 (4)) 5 6 ((7))) 4) 4)
(check-expect (nested-ref '((0 "fizz" 2) ("buzz" (4)) "fizz" 6 (("buzz"))) 3) "buzz")

;; Part F
;; nested-filter filters out all the values in the list
;; that are false by the input predicate

;; Examples:
(check-expect (nested-filter boolean? '(true false true true)) empty)
(check-expect (nested-filter symbol? '(haha bahaha)) '(haha bahaha))

;; nested-filter: (X -> Bool) (nested-listof X) -> (listof X)

(define (nested-filter pred? lst)
  (cond
    [(empty? lst) '()]
    [(list? (first lst)) (cons (nested-filter pred? (first lst))
                               (nested-filter pred? (rest lst)))]
    [(pred? (first lst)) (cons (first lst)
                               (nested-filter pred? (rest lst))) ]
    [else (nested-filter pred? (rest lst))]))

;; Tests:
(check-expect (nested-filter symbol? '()) empty)
(check-expect (nested-filter even? '(8 9 ((10)) 2)) (list 8 10 2))
(check-expect (nested-filter number? '(0 1 2)) '(0 1 2))
(check-expect (nested-filter number? empty) empty)

;; Part G
;; Examples:
(check-expect (nested-cleanup '((1) (((3) () ())) 56 () () 7)) '((1) (((3))) 56 7))
(check-expect (nested-cleanup '(() (() ()) ()())) false)
(check-expect (nested-cleanup '(1 () 3 () () 8 (()))) '(1 3 8))











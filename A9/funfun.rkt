;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 09, Problem 4
;; ***************************************************

;; Part A
;; multi-apply takes in a list of functions and a value. Apply these functions from left
;; to right on the value

;; Examples:
(check-expect (multi-apply (list add1 sub1 add1 add1) 9) 11)
(check-expect (multi-apply '() 9) 9)

;; multi-apply: (listof (X -> X)) X -> X

(define (multi-apply lof val)
  (foldl (lambda (func ele) (func ele)) val lof))

;; Tests:
(check-expect (multi-apply (list add1 sub1 add1 add1) 9) 11)
(check-expect (multi-apply '() 9) 9)
(check-expect (multi-apply (list add1 add1 add1) 9) 12)



;; Part B
;; aop takes in a natural number n and produces a function that takes in x, that follows
;; the pattern (x^n + x^ (n-1) + ... (x^0). 

;; Examples:
(check-expect ((aop 3) 2) 15)
(check-expect ((aop 1) 10) 11)

;; aop: Int -> (X -> X)

(define (aop n)
  (lambda (num)
    (foldl (lambda (x sum) (+ (expt num x) sum)) 0
           (build-list (+ n 1) (lambda (x) x)))))

;; Tests:
(check-expect ((aop 4) 2) 31)
(check-expect ((aop 0) 3) 1)
(check-expect ((aop -1) 10) 0)


;; Part C
;; multi-compose takes a list of functions and returns the combined function from the list
;; of functions

;; Examples:
(check-expect ((multi-compose (list add1 add1)) 6) 8)
(check-expect ((multi-compose (list add1 sub1)) 7) 7)
(check-expect ((multi-compose (list add1 sqr)) 9) 100)

;; multi-compose: (listof (X -> X)) -> (X -> X)

(define (multi-compose lof)
  (lambda (val)
    (foldl (lambda (func ele) (func ele)) val lof)))

;; Tests:
(check-expect ((multi-compose '()) 6) 6)
(check-expect ((multi-compose (list sqrt sqr)) 9) 9)





                   

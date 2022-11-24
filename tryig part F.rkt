;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tryig part F|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Part G DOES NOT WORK
;; Examples:
(check-expect (nested-cleanup '((1) (((3) () ())) 56 () () 7)) '((1) (((3))) 56 7))
(check-expect (nested-cleanup '(() (() ()) ()())) false)
(check-expect (nested-cleanup '(1 () 3 () () 8 (()))) '(1 3 8))

(define (nested-cleanup lst)
  (cond
    [(empty? lst) false]
    [else (cons (first lst) (nested-cleanup (rest lst)))]))

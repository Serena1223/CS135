;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname nat-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 04, Problem 6
;; ***************************************************
;;

;; Part A
;; (nat->list n) converts natural numer n into a list of digits

;; Examples
(check-expect (nat->list 9042) (cons 2 (cons 4 (cons 0 (cons 9 empty)))))
(check-expect (nat->list 8070) (cons 0 (cons 7 (cons 0 (cons 8 empty)))))

;; nat->list: Nat -> (listof Digits)

(define (nat->list n)
  (cond
    [(< n 10) (cons n empty)]
    [else (cons (remainder n 10) (nat->list (quotient n 10)))]))

;; Tests
(check-expect (nat->list 99) (cons 9 (cons 9 empty)))
(check-expect (nat->list 900) (cons 0 (cons 0 (cons 9 empty))))
(check-expect (nat->list 13) (cons 3 (cons 1 empty)))

;; Part  B
;; (list->nat lon) converts a list of digits back into its equivalent natural number

;; Examples
(check-expect (list->nat (cons 8 (cons 0 (cons 3 empty)))) 308)
(check-expect (list->nat (cons 1 (cons 0 (cons 1 empty)))) 101)

;; list->nat: (listof Digits) -> Nat

(define (list->nat lon)
  (cond
    [(empty? lon) 0]
    ;;[(empty? (rest lon)) (+(first lon) ()]
    [else (+ (first lon) (* 10 (list->nat (rest lon))))]))

;; Tests
(check-expect (list->nat (cons 9 (cons 2 (cons 4 empty)))) 429)
(check-expect (list->nat (cons 8 (cons 0 (cons 1 (cons 7 empty))))) 7108)
(check-expect (list->nat (cons 0 (cons 1 empty))) 10)





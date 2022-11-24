;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 06, Problem 3
;; ***************************************************
;;

;; Part A
;; (union s1 s2) produces a list without duplicates of the union of set 1 and set 2
;; Examples:
(check-expect (union (list 1 3 4) (list 1 5)) (list 1 3 4 5)) 
(check-expect (union (list 1 3 4 9) (list 1 3)) (list 1 3 4 9))

;; union: (setof Nat) (setof Nat) -> (setof Nat)

(define (union s1 s2)
  (cond
    [(and (empty? s1) (empty? s2)) empty]
    [(empty? s1) s2]
    [(empty? s2) s1]
    [(= (first s1) (first s2)) (cons (first s1) (union (rest s1) (rest s2)))]
    [(< (first s1) (first s2)) (cons (first s1) (union (rest s1) s2))]
    [else (cons (first s2) (union s1 (rest s2)))]))
    

;; Tests:
(check-expect (union (list 1 4) (list 1 2 5 8 19)) (list 1 2 4 5 8 19))
(check-expect (union (list 8 10) (list 1 2 5 8 19)) (list 1 2 5 8 10 19))


;; Part B
;; (intersection s1 s2) produces the overlaps between set 1 and 2
;; Examples:
(check-expect (intersection (list 1 3 4) (list 1 5)) (list 1))
(check-expect (intersection (list 1 6 9) (list 1 3 9)) (list 1 9))

;; intersection: (setof Nat) (setof Nat) -> (setof Nat)

(define (intersection s1 s2)
  (cond
    [(empty? s1) empty]
    [(empty? s2) empty]
    [(= (first s1) (first s2)) (cons (first s1) (intersection (rest s1) (rest s2)))]
    [(< (first s1) (first s2)) (intersection (rest s1) s2)]
    [else (intersection s1 (rest s2))]))

;; Tests:
(check-expect (intersection (list 1 3 9 11 30) (list 1 9 30)) (list 1 9 30))
(check-expect (intersection (list -1 3 10 15) (list 1 3 10)) (list 3 10))
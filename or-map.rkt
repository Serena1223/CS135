;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname or-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 08, Problem 1
;; ***************************************************
;;
;; Part A
;; (my-ormap) takes checks if any element in the list is true for pred?

;; Examples:
(check-expect (my-ormap symbol? '(b 0 -99)) true)
(check-expect (my-ormap symbol? '("h" "9" 9)) false)

;; my-ormap: (listof X) (X -> Bool) -> Bool

(define (my-ormap pred? lst)
  (cond
    [(empty? lst) false]
    [(pred? (first lst)) true]
    [else (my-ormap pred? (rest lst))]))

;; Tests
(check-expect (my-ormap string=? '()) false)
(check-expect (my-ormap even? '(24 13 0)) true)
  
;; Part B
;; (pred?-ormap) takes a list of predicates and evaluate
;; if the value is true for any of the predicates in the list

;; Examples:
(check-expect (pred?-ormap 0 (list symbol? zero? odd?)) true)
(check-expect (pred?-ormap -3 (list symbol? zero? odd?)) true)
(check-expect (pred?-ormap 2 (list symbol? zero? odd?)) false)

;; pred?-ormap: Any (list of (X -> Bool)) -> Bool

(define (pred?-ormap val boolst)
  (cond
    [(empty? boolst) false]
    [((first boolst) val) true]
    [else (pred?-ormap val (rest boolst))]))

;; Tests









  



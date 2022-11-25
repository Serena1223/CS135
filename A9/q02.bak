;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 09, Problem 2
;; ***************************************************

;; Part A
;; Examples:
(check-expect (alphanumeric-only '("efew9ge01" "+%^dhelo" "*?")) '("efew9ge01"))
(check-expect (alphanumeric-only '("efew9ge01" "90" "+%^?" "*")) '("efew9ge01" "90"))
(check-expect (alphanumeric-only '()) '())

;; Part B
;; Examples:
(check-expect (remove-outliers '(2 8 0 7 10 8 34)) '(2 8 0 7 10 8))
(check-expect (remove-outliers '(0 0 0 0)) '(0 0 0 0))
(check-expect (remove-outliers '(-9 0 0 0)) '(-9 0 0 0))
(check-expect (remove-outliers '()) '())

;; Part C
(check-expect (zero-fill "financebro") "financebro0000000000")
(check-expect (zero-fill "csbro") "csbro000000000000000")
(check-expect (zero-fill "     ") "     000000000000000")

;; Part D
(check-expect (remove-duplicates (list 0 3 0 4 5 1 3 1)) (list 0 3 4 5 1))
(check-expect (remove-duplicates (list 1 2 3 1 3 1)) (list 1 2 3))
(check-expect (remove-duplicates empty) empty)






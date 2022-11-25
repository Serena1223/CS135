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
(check-expect (multi-apply (list add1 sub1 add1 add1) 9) 11)
(check-expect (multi-apply '() 9) 9)

;; Part B
(check-expect ((aop 3) 2) 15)
(check-expect ((aop 1) 10) 11)


;; Part C
(check-expect ((multi-compose (list add1 add1)) 6) 8)
(check-expect ((multi-compose (list add1 sub1)) 7) 7)
(check-expect ((multi-compose (list add1 sqrt)) 9) 4)




                   

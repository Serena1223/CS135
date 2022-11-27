;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bw-images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 09, Problem 3
;; ***************************************************

(define diamond-image
  '((0 1 0)
    (1 0 1)
    (0 1 0)))


(define asym-image
  '((0 1 1 0)
    (0 1 0 0)
    (0 0 0 0)
    (0 0 0 0)))
  

;; Part A
;; invert-image takes in a 2D-Image and invert it, that is, 0 becomes 1 and 1 becomes 0
;; Examples: 
(check-expect (invert diamond-image)
              '((1 0 1)
                (0 1 0)
                (1 0 1)))
(check-expect (invert asym-image)
              '((1 0 0 1)
                (1 0 1 1)
                (1 1 1 1)
                (1 1 1 1)))

;; Part B
;; reflect-x-axis reflects a 2D-image across the x-axis
(check-expect (reflect-x-axis diamond-image)
              '((0 1 0)
                (1 0 1)
                (0 1 0)))

(check-expect (reflect-x-axis asym-image)
              '((0 0 0 0)
                (0 0 0 0)
                (0 1 0 0)
                (0 1 1 0)))

;; Part C
;; reflect-y-axis reflects a 2D-image across the y-axis
(check-expect (reflect-y-axis diamond-image)
              '((0 1 0)
                (1 0 1)
                (0 1 0)))

(check-expect (reflect-y-axis asym-image)
              '((0 1 1 0)
                (0 0 1 0)
                (0 0 0 0)
                (0 0 0 0)))
;; Part D
;; transpose will transpose the 2D-Image, that is, its rows become colums and its
;; columns become rows

(check-expect (transpose diamond-image)
              '((0 1 0)
                (1 0 1)
                (0 1 0)))

(check-expect (transpose asym-image)
              '((0 0 0 0)
                (1 1 0 0)
                (1 0 0 0)
                (0 0 0 0)))
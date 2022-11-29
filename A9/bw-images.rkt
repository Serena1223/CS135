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


(define new-image
  '((0 1 1 0)
    (0 1 0 0)
    (0 0 1 1)
    (0 0 0 0)))
  
(define cool-image
  '((0 1 0 0)
    (1 0 0 1)
    (0 0 1 1)
    (0 1 1 1)))

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

;; invert: 2D-Image -> 2D-Image

(define (invert img)
  (local [(define (invert-row row)
            (map (lambda (x)
                   (cond [(zero? x) 1]
                         [else 0])) row))]
    (map invert-row img)))

;; Tests:
(check-expect (invert '((1))) '((0)))
(check-expect (invert new-image)
  '((1 0 0 1)
    (1 0 1 1)
    (1 1 0 0)
    (1 1 1 1)))

;; Part B
;; reflect-x-axis reflects a 2D-image across the x-axis

;; Examples:
(check-expect (reflect-x-axis diamond-image)
              '((0 1 0)
                (1 0 1)
                (0 1 0)))

(check-expect (reflect-x-axis asym-image)
              '((0 0 0 0)
                (0 0 0 0)
                (0 1 0 0)
                (0 1 1 0)))

;; reflect-x-axis: 2D-Image -> 2D-Image 

(define (reflect-x-axis img)
  (foldl cons empty img))

;; Tests:
(check-expect (reflect-x-axis cool-image)
              '((0 1 1 1)
                (0 0 1 1)
                (1 0 0 1)
                (0 1 0 0)))

(check-expect (reflect-x-axis new-image)
              '((0 0 0 0)
                (0 0 1 1)
                (0 1 0 0)
                (0 1 1 0)))

(check-expect (reflect-x-axis '((0 0)))  '((0 0)))

;; Part C
;; reflect-y-axis reflects a 2D-image across the y-axis

;; Examples: 
(check-expect (reflect-y-axis diamond-image)
              '((0 1 0)
                (1 0 1)
                (0 1 0)))

(check-expect (reflect-y-axis asym-image)
              '((0 1 1 0)
                (0 0 1 0)
                (0 0 0 0)
                (0 0 0 0)))

;; reflect-y-axis: 2D-Image -> 2D-Image

(define (reflect-y-axis img)
  (local [(define (reverse-row row)
            (foldl cons empty row))]
    (map reverse-row img)))

;; Tests:
(check-expect (reflect-y-axis new-image)
              '((0 1 1 0)
                (0 0 1 0)
                (1 1 0 0)
                (0 0 0 0)))

(check-expect (reflect-y-axis cool-image)
              '((0 0 1 0)
                (1 0 0 1)
                (1 1 0 0)
                (1 1 1 0)))

(check-expect (reflect-y-axis '((0))) '((0)))
(check-expect (reflect-y-axis '((0) (0)))  '((0) (0)))
(check-expect (reflect-y-axis '((0) (1)))  '((0) (1)))


;; Part D
;; transpose will transpose the 2D-Image, that is, its rows become colums and its
;; columns become rows

;; Examples:
(check-expect (transpose diamond-image)
              '((0 1 0)
                (1 0 1)
                (0 1 0)))

(check-expect (transpose asym-image)
              '((0 0 0 0)
                (1 1 0 0)
                (1 0 0 0)
                (0 0 0 0)))

;; transpose: 2D-Image -> 2D-Image

(define (transpose img)
  ;; getting the nth column into a list
  (local [(define (nth-col lsts n)
            (foldr (lambda (row rror)
                     (cons (list-ref row n) rror)) ;; build list by consing each row's nth number
                   empty lsts))]

    (foldr (lambda (n rror) (cons (nth-col img n) rror))
           empty
           ;; build list based on how many rows/cols there are in the image (dimension)
           (build-list (length img)(lambda (x) x)))))


;; Tests:
(check-expect (transpose new-image)
              '((0 0 0 0)
                (1 1 0 0)
                (1 0 1 0)
                (0 0 1 0)))

(check-expect (transpose '((0 0) (1 1)))
              '((0 1) (0 1)))

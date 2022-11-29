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
;; alphanumeric-only takes in a list of strings and only keeps the elements with
;; alphanumerics only

;; Examples:
(check-expect (alphanumeric-only '("efew9ge01" "+%^dhelo" "*?")) '("efew9ge01"))
(check-expect (alphanumeric-only '("efew9ge01" "90" "+%^?" "*")) '("efew9ge01" "90"))
(check-expect (alphanumeric-only '()) '())

;; alphanumeric-only: (listof String) -> (listof String)

(define (alphanumeric-only lst)
  (filter
   (lambda (str)
     (foldl (lambda (c rror) (and (or (char-alphabetic? c)
                                      (char-numeric? c))
                                  rror))
            true (string->list str)))
   lst))

;; Tests:
(check-expect (alphanumeric-only '("efew9ge01" "poopopopoo" "uwuwu"))
              '("efew9ge01" "poopopopoo" "uwuwu"))
(check-expect (alphanumeric-only '("***" "" "+%^?" "*")) '(""))
(check-expect (alphanumeric-only '("" "")) '("" ""))
(check-expect (alphanumeric-only '("hi" "hi" "91%" "91")) '("hi" "hi" "91"))



;; Part B
;; remove-outliers removes the outliers (defined as any element more than 1 Standard Deviation
;; more or less than the mean) in a list of numbers

;; Examples:
(check-expect (remove-outliers '(2 8 0 7 10 8 34)) '(2 8 0 7 10 8))
(check-expect (remove-outliers '(0 0 0 0)) '(0 0 0 0))

;;remove-outliers: (listof Num) -> (listof Num)

(define (remove-outliers lst)
  (local [(define n (foldr (lambda (x y)
                             (add1 y)) 0 lst))
          (define (find-mean lst)
            (/ (foldr + 0 lst) n))
          (define mean (find-mean lst))
          
          (define (find-SD lst)
            (sqrt (/ (foldr (lambda (x rror)
                                   (+ (sqr (- x mean)) rror)) 0 lst) n)))
          (define SD (find-SD lst))
          
          (define lower-bound (- mean SD))
          (define upper-bound (+ mean SD))]
   
  (filter (lambda (item) (and (>= item lower-bound) (<= item upper-bound))) lst)))

;; Tests:
(check-expect (remove-outliers '(1)) '(1))
(check-expect (remove-outliers '(0 99 -9.9 9.9)) '(0 -9.9 9.9))
(check-expect (remove-outliers '(0 999 -999)) '(0))

;; Part C
;; zero-fill takes in a string and adds 0s to its tail until the string is 20 characters long

;; Examples
(check-expect (zero-fill "financebro") "financebro0000000000")
(check-expect (zero-fill "csbro") "csbro000000000000000")
(check-expect (zero-fill "     ") "     000000000000000")

;; zero-fill: Str -> Str
;;      Requires: (length str) <= 20

(define (zero-fill str)
  (list->string (build-list 20 (lambda (x) (cond
                                             [(< x (length (string->list str)))
                                              (list-ref (string->list str) x)]
                                             [else #\0])))))
;; Tests:
(check-expect (zero-fill "") "00000000000000000000")
(check-expect (zero-fill "189  00 ") "189  00 000000000000")
(check-expect (zero-fill "1 2 0") "1 2 0000000000000000")


;; Part D
(check-expect (remove-duplicates (list 0 3 0 4 5 1 3 1)) (list 0 3 4 5 1))
(check-expect (remove-duplicates (list 1 2 3 1 3 1)) (list 1 2 3))
(check-expect (remove-duplicates empty) empty)






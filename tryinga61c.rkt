;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tryinga61c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Part C
;; (sel-sort) sorts a list of numbers into non-decreasing order. 

;; sel-sort: (listof Num) -> (listof Num)
(check-expect (sel-sort (list 4 51 0 4)) (list 0 4 4 51))
(check-expect (sel-sort (list 4 0 4)) (list 0 4 4))

(define (sel-sort lon)
  (sel-sort/sf (smallest-first lon) empty))

;; Sorts a list from smallest to greatest
(define (sel-sort/sf lon acclist)
  (cond
    [(empty? (rest lon)) (cons (first lon) acclist)]
    [else (sel-sort/sf (smallest-first (rest lon)) (cons (first lon) acclist))]))

(define (sel-sort/sf lon)
  (cond
    [(empty? (rest lon)) empty]
    [else (cons (first lon) (sel-sort/sf (smallest-first(rest lon))))]
    )
  )



;; (smallest-first) takes a list of numbers and outputs the list with specifically, the
;; smallest number in the beginning of the list.

;; Examples:
(check-expect (smallest-first (list 1 9 3 0 8)) (list 0 8 1 3 9))
(check-expect (smallest-first (list 0)) (list 0))
(check-expect (smallest-first (list 4 1 9 0)) (list 0 4 1 9))
(check-expect (smallest-first (list 1 9 0)) (list 0 1 9))

;; smallest-first: (listof Num) -> (listof Num)

(define (smallest-first lon)
  (smallest-first/acc (rest lon) (first lon) empty))

;; Tests:
(check-expect (smallest-first (list -9 3 5 1 2)) (list -9 2 1 5 3))
(check-expect (smallest-first (list 12 1)) (list 1 12))

;; (smallest-first/acc) is a helper for smallest-first
;; using a lon, accumulator, and accumulator list 

;; Examples
(check-expect (smallest-first/acc (list 5 0 3 8) 5 empty) (list 0 8 3 5 5)) 
(check-expect (smallest-first/acc (list -3 -10 9 0) 0 empty) (list -10 0 9 -3 0))
(check-expect (smallest-first/acc (list -3 -1 9 10) 3 empty) (list -3 10 9 -1 3))

;; smallest-first/acc: (listof Num) Num (listof Num) -> (listof Num)

(define (smallest-first/acc lon smallest new-list)
  (cond
    [(empty? lon) (cons smallest new-list)]
    [(< (first lon) smallest) (smallest-first/acc (rest lon) (first lon)
                                                  (cons smallest new-list))]
    [else (smallest-first/acc (rest lon) smallest (cons (first lon) new-list))]))
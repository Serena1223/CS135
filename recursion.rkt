;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 06, Problem 1
;; ***************************************************
;;

;; Part A
;; (in-range a b lon) scans the lon and outputs the number of elements in [a, b]

;; Examples:
(check-expect (in-range 3 5 (list 1 5 3 9)) 2)
(check-expect (in-range 1 100 (list 150 23 52.1 -9)) 2)

;; in-range: Num Num (listof Num) -> Nat

(define (in-range a b lon)
  (in-range/acc a b lon 0))

(define (in-range/acc a b lon acc)
  (cond
    [(empty? lon) acc]
    [(and (<= (min a b) (first lon)) (>= (max a b) (first lon)))
     (in-range/acc a b (rest lon) (+ acc 1))]
    [else (in-range/acc a b (rest lon) acc)]))


;; Tests:
(check-expect (in-range 3 9 (list 1 5 7 9 10)) 3)
(check-expect (in-range 10 3 (list 1 5 9 7 10))4)
(check-expect (in-range -11 11 (list 0 -10 8 1 15 -19))4)

;; Part B
;;(spread) produces the positive difference between the largest and smallest elements in a list. 
;; Examples:
(check-expect (spread (list 2 1 5 1 3)) 4)
(check-expect (spread (list 3 -3 0 3)) 6)

;; spread: (listof Num) -> Num

(define (spread lon)
  (spread/acc lon (first lon) (first lon)))

(define (spread/acc lon min-sofar max-sofar)
  (cond
    [(empty? lon) (abs(- min-sofar max-sofar))]
    ;; check if it's smaller than min so far
    [(<= (first lon) min-sofar)
     (spread/acc (rest lon) (first lon) max-sofar)]
    ;; check if it's greater than max so far
    [(>= (first lon) max-sofar)
     (spread/acc (rest lon) min-sofar (first lon))]
    ;; else just run it again
    [else (spread/acc (rest lon) min-sofar max-sofar)]))

;; Tests:
(check-expect (spread (list 1 1 1)) 0)
(check-expect (spread (list -100 99 0 160)) 260)
(check-expect (spread (list -1 76 0 3)) 77)


;; Part C
;; (sel-sort) sorts a list of numbers into non-decreasing order. 

;; sel-sort: (listof Num) -> (listof Num)

;; Examples
(check-expect (sel-sort (list 4 51 0 4)) (list 0 4 4 51))
(check-expect (sel-sort (list 4 0 4)) (list 0 4 4))

(define (sel-sort lon)
  (sel-sort/sf (smallest-first lon)))

;; Tests:
(check-expect (sel-sort (list -4 5 1 2)) (list -4 1 2 5))
(check-expect (sel-sort (list 3 1 6)) (list 1 3 6))

;; (sel-sort/sf) is a helper function for sel-sort. It takes in a list that has the first
;; element being the smallest.

;; sel-sort/sf: (listof Num) -> (listof Num)

;; Examples:
(check-expect (sel-sort/sf (list -4 5 2)) (list -4 2 5))
(check-expect (sel-sort/sf (list 1 5 1 4)) (list 1 1 4 5))

(define (sel-sort/sf lon)
  (cond
    [(empty? (rest lon)) (cons (first lon) empty)]
    [else (cons (first lon) (sel-sort/sf (smallest-first(rest lon))))]))


;; (smallest-first) takes a list of numbers and outputs the list with specifically, the
;; smallest number in the beginning of the list.

;; Examples:
(check-expect (smallest-first (list 1 9 3 0 8)) (list 0 8 1 3 9))
(check-expect (smallest-first (list 0)) (list 0))
(check-expect (smallest-first (list 4 1 9 0)) (list 0 1 9 4))
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






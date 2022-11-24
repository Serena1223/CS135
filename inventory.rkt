;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 06, Problem 2
;; ***************************************************
;;

;; Part A
;; ev is a structure for type EV.
;; a EV is  (make-ev model year price mileage mpge)
;; Requires: model is a string.
;; Requires: year is a year is a natural number of the year.
;; Requires: price is a number
;; Requires: mileage is a positive number
;; Requires: mpge is a positive number

(define-struct ev (model year price mileage mpge))


;; Part B
;; (adjust-prices) lowers every element in a list by a fixed percentage
;; Examples:
(check-expect (adjust-prices (list (make-ev "Mercedes" 2022 100 1 3)) 0.2)
                             (list (make-ev "Mercedes" 2022 120 1 3)))
      
;; adjust-prices: (listof EV) Num -> (listof EV)

(define (adjust-prices loev change)
  (cond
    [(empty? loev) empty]
    [else (cons (make-ev (ev-model (first loev))
                         (ev-year (first loev))
                         (* (+ 1 change) (ev-price (first loev)))
                         (ev-mileage (first loev))
                         (ev-mpge (first loev))) (adjust-prices (rest loev) change))]))
;; Tests:
(check-expect (adjust-prices (list
                              (make-ev "Redbull" 1300 1000 90 9)
                              (make-ev "Tesla" 1990 2000 10 8)) -0.1)
              (list (make-ev "Redbull" 1300 900 90 9)
                    (make-ev "Tesla" 1990 1800 10 8)))

(check-expect (adjust-prices (list (make-ev "Hello Car" 2000 300 5 3)) 0.1)
                             (list (make-ev "Hello Car" 2000 330 5 3)))



;; Part C
(define models (list "Alpha Tauri" "Mercedes Lewis" "F1 Car" "Red Bull"))
(define years (list 1999 2013 2020 1909))
(define prices (list 1000 1523524 34131 1251251))
(define mileage (list 34353 1523524 3413 134153))
(define mpge (list 10101 53535 231 66342))

;;(build-inventory) has a list for each parameter similar to a table.

;; Examples:
(check-expect (build-inventory models years prices mileage mpge)
              (list
               (make-ev "Red Bull" 1909 1251251 134153 66342)
               (make-ev "F1 Car" 2020 34131 3413 231)
               (make-ev "Mercedes Lewis" 2013 1523524 1523524 53535)
               (make-ev "Alpha Tauri" 1999 1000 34353 10101)))

;; build-inventory: (listof Str) (listof Nat) (listof Num) (listof Num) (listof Num) -> (listof EV)

(define (build-inventory/acc models years prices mileage mpge acc)
  (cond
    [(empty? models) acc]
    ;; the idea is, call the function for the rest of the list on an
    ;; accumulator that has the first element added to it. 
    [else (build-inventory/acc (rest models) (rest years) (rest prices) (rest mileage) (rest mpge)
           (cons (make-ev (first models) (first years) (first prices) (first mileage) (first mpge)) acc))]))

(define (build-inventory models years prices mileage mpge)
  (build-inventory/acc models years prices mileage mpge empty))

;; Tests:
(check-expect (build-inventory (list "Gas car" "Tesla" "BMW")
                               (list 2022 2021 2022)
                               (list 400 800 100)
                               (list 1290 1800 9110)
                               (list 198 150 131))
              (list (make-ev "BMW" 2022 100 9110 131)
                    (make-ev "Tesla" 2021 800 1800 150)
                    (make-ev "Gas car" 2022 400 1290 198)))

;; Part D
;;(compare-ev) compares two EVs based on year, mpge, and mileage.

;; Examples:
(check-expect (compare-ev (make-ev "bcar" 1999 101010 9000 1000)
                          (make-ev "hi car" 2021 2 100 34343)) 'lt)
(check-expect (compare-ev (make-ev "Merc car" 2023 101010 9000 1000)
                          (make-ev "Red bull" 2023 200 100 34343)) 'lt)

;; compare-ev: EV EV -> (anyof 'lt 'eq 'gt)

(define (compare-ev ev1 ev2)
  (cond
    [(> (ev-year ev1) (ev-year ev2)) 'gt]
    [(< (ev-year ev1) (ev-year ev2)) 'lt]
    [(< (ev-mpge ev1) (ev-mpge ev2)) 'lt]
    [(> (ev-mpge ev1) (ev-mpge ev2)) 'gt]
    [(< (ev-mileage ev1) (ev-mileage ev2)) 'gt]
    [(> (ev-mileage ev1) (ev-mileage ev2)) 'lt]
    [else 'eq]))

;; Tests:
(check-expect (compare-ev (make-ev "Merc car" 2023 101010 9000 1000)
                          (make-ev "Red bull" 2021 2 100 34343)) 'gt)
(check-expect (compare-ev (make-ev "Merc car" 2023 200 100 1000)
                          (make-ev "Red bull" 2023 200 100 1000)) 'eq)
(check-expect (compare-ev (make-ev "Merc car" 2023 200 200 1000)
                          (make-ev "Red bull" 2023 200 100 1000)) 'lt)


;; Part e
;; (sort-evs)

;; Examples:
(check-expect (sort-evs (list (make-ev "Alpha Tauri" 1999 1000 34353 10101)
                              (make-ev "Mercedes Lewis" 2013 1523524 25 53535)
                              (make-ev "F1 Car" 2020 53000 34131 231)
                              (make-ev "Red Bull" 1909 1251251 34153 66342)))
                        (list (make-ev "F1 Car" 2020 53000 34131 231)
                              (make-ev "Mercedes Lewis" 2013 1523524 25 53535)
                              (make-ev "Alpha Tauri" 1999 1000 34353 10101)
                              (make-ev "Red Bull" 1909 1251251 34153 66342)))

;; sort-evs: (listof EV) -> (listof EV)


(define (sort-evs lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon) (sort-evs (rest lon)))]))


;; helper function comments not needed.
(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        ;; if n is better than the first of the list, add it to list
        [(symbol=? (compare-ev n (first slon)) 'gt) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))

;; Tests:
(check-expect (sort-evs (list (make-ev "Alpha Tauri" 1999 1000 34353 10101)
                              (make-ev "Mercedes Lewis" 2013 1523524 25 53535)
                              (make-ev "F1 Car" 2020 53000 34131 231)
                              (make-ev "Red Bull" 1909 1251251 34153 66342)))
                        (list (make-ev "F1 Car" 2020 53000 34131 231)
                              (make-ev "Mercedes Lewis" 2013 1523524 25 53535)
                              (make-ev "Alpha Tauri" 1999 1000 34353 10101)
                              (make-ev "Red Bull" 1909 1251251 34153 66342)))

             
              

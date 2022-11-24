;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hot-dog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 04, Problem 3
;; ***************************************************
;;

;; Part A
;; (contains-hot-dog? loa) determins if a list contains 'hot-dog

;; Examples
(check-expect (contains-hot-dog?
               (cons 'hot-dog (cons "hello" (cons 'yo empty)))) true)
(check-expect (contains-hot-dog?
               (cons 1 (cons "2" (cons 'hotdoggo empty)))) false)

;; contains-hot-dog?: (listof Any) -> Bool

(define (contains-hot-dog? loa)
  (cond
    [(empty? loa) false]
    [(and (symbol? (first loa)) (symbol=? (first loa) 'hot-dog)) true]
    [else (contains-hot-dog? (rest loa))]))

;; Tests:
(check-expect (contains-hot-dog? (cons false
                                       (cons "true" (cons true empty)))) false)
(check-expect (contains-hot-dog? (cons 'hot-dog empty)) true)
(check-expect (contains-hot-dog?
               (cons 634 (cons "0" (cons 'hot-dog (cons false empty))))) true)



;; Part B
;; (spells-hot-dog? s) checks if the letters in a string can spell "hot dog"

;; Examples:
(check-expect (spells-hot-dog? "abcdTefg HooG too") true)
(check-expect (spells-hot-dog? "no dog") false)

;; spells-hot-dog?: Str -> Bool

(define (spells-hot-dog? s) ;; specific to the hot dog case
  (and
   (check-contents 1 #\h (string->list s))
   (check-contents 1 #\t (string->list s))
   (check-contents 1 #\d (string->list s))
   (check-contents 1 #\g (string->list s))
   (check-contents 2 #\o (string->list s))
   (check-contents 1 #\space (string->list s))))

;; Tests:
(check-expect (spells-hot-dog? "hatGOOldagweg ") true)
(check-expect (spells-hot-dog? "hotgot") false)
(check-expect (spells-hot-dog? "g otho t") false)



;; (check-contents n item list) is a general
;; helper function that checks if a list contains n items

;; Examples:
(check-expect (check-contents 1 #\h (cons #\d (cons #\h empty))) true)
(check-expect (check-contents 0 #\a (cons #\o (cons #\space empty))) true)

;; check-contents: Nat Sym (listof Sym) -> Bool

(define (check-contents n item list)
  (cond
    [(zero? n) true]
    [(empty? list) false]
    ;;non empty list that matches and requires at least 1 item
    [(char=? (char-downcase(first list)) item) (check-contents (sub1 n) item (rest list))]
    ;;non empty list that doesn't match and requires at least 1 item
    [else (check-contents n item (rest list))]))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fizz-buzz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 04, Problem 5
;; ***************************************************
;;

;; (fizz-buzz start end fizz buzz) outputs a list of integers based on divisibility
;; by fizz and buzz. It follows the details of Waterloo Fizz Buzz

;; Examples:
(check-expect (fizz-buzz 1 5 2 3)
              (cons 1 (cons 'fizz (cons 'buzz (cons 'fizz (cons 5 empty))))))
(check-expect (fizz-buzz 10 14 1 2)
              (cons 'honk (cons 'fizz (cons 'honk (cons 'fizz (cons 'honk empty))))))

;; fizz-buzz: Num Num Nat Nat -> (listof (anyof Int Sym))

(define (fizz-buzz start end fizz buzz)
  (cond
    [(> start end) empty]
    [(and (= (remainder start fizz) 0)
         (= (remainder start buzz) 0))
         (cons 'honk (fizz-buzz (add1 start) end fizz buzz))]
    [(= (remainder start fizz) 0)
     (cons 'fizz (fizz-buzz (add1 start) end fizz buzz))]
    [(= (remainder start buzz) 0)
     (cons 'buzz (fizz-buzz (add1 start) end fizz buzz))]
    [else (cons start (fizz-buzz (add1 start) end fizz buzz))]))

;; Tests
(check-expect (fizz-buzz 1 2 3 4)
              (cons 1 (cons 2 empty)))
(check-expect (fizz-buzz 5 8 3 4)
              (cons 5 (cons 'fizz (cons 7 (cons 'buzz empty)))))
(check-expect (fizz-buzz 97 100 7 3)
              (cons 97 (cons 'fizz (cons 'buzz (cons 100 empty)))))


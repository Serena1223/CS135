;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname waterloo2-poker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 03, Problem 2
;; ***************************************************
;;

;; TODO: move helpers to bottom
(define (rank card)
  (first card))

;; Problem 2 part a
;; Write a function ordinality which consumes a Card c and produces the ordinality of c.
;; 
;; Examples:
(check-expect (ordinality (cons 3(cons 'S empty))) 3)
(check-expect (ordinality (cons 'K (cons 'S empty))) 13)
(check-expect (ordinality (cons 9(cons 'S empty))) 9)
(define (ordinality c)
  (cond
    [(number? (rank c)) (rank c)]
    [(symbol? (rank c)) (cond
                          [(symbol=? (rank c) 'J) 11]
                          [(symbol=? (rank c) 'Q) 12]
                          [(symbol=? (rank c) 'K) 13]
                          [(symbol=? (rank c) 'A) 14]
                          [else -1])]
    [else -1]))
;; Tests:

;; Card -> Int ????


;; Problem 2 part b
;; Write a function strength which consumes a Hand h and produces the strength of h as
;;a number according to the table above (i.e., one of: 0,1,2,3,4).
(check-expect (strength (cons (cons 'J (cons 'H empty))
                             (cons (cons 10 (cons 'H empty)) empty))) 4)
(check-expect (strength (cons (cons 3 (cons 'C empty))
                             (cons (cons 'A (cons 'C empty)) empty))) 1)
(check-expect (strength (cons (cons 'K (cons 'S empty))
                             (cons (cons 'Q (cons 'D empty)) empty))) 2)


;; Problem 2 part c
;; Write a predicate function hand<? which consumes two Hands h1 and h2 and produces
;;true if h2 is a better Hand than h1, and false otherwise.
(check-expect (hand<?
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 'J (cons 'S empty))
                    (cons (cons 'Q (cons 'D empty)) empty))) true)

(check-expect (hand<?
              (cons (cons 'J (cons 'H empty))
                    (cons (cons 10 (cons 'H empty)) empty))
              (cons (cons 'A (cons 'S empty))
                    (cons (cons 'A (cons 'D empty)) empty))) false)

(check-expect (hand<?
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 'A (cons 'S empty))
                    (cons (cons 'A (cons 'D empty)) empty))) true)
(define (hand<? h1 h2)
  (< (strength h1) (strength h2)))

  
;; Problem 2 part d
;;Write a function winner which consumes two Hands h1 and h2 and produces 'hand1
;;if h1 is a better Hand than h2 or 'hand2 if h2 is a better Hand than h1, and 'tie if both
;;Hands are equivalent.
(check-expect (winner
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 'J (cons 'S empty))
                    (cons (cons 'Q (cons 'D empty)) empty))) 'hand2)

(check-expect (winner
              (cons (cons 'J (cons 'H empty))
                    (cons (cons 10 (cons 'H empty)) empty))
              (cons (cons 'A (cons 'S empty))
                    (cons (cons 'A (cons 'D empty)) empty))) 'hand1)

(check-expect (winner
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))) 'tie)
(define (winner h1 h2)
  (cond
    [(= (strength h1) (strength h2)) 'tie]
    [(hand<? h1 h2) 'hand2]
    [else 'hand1]))

;; Problem 2 part e
;; Write a function winner which consumes two Hands h1 and h2 and produces 'hand1
;;if h1 is a better Hand than h2 or 'hand2 if h2 is a better Hand than h1, and 'tie if both
;;Hands are equivalent.

;; helper function
(define (count-list loc)
  (cond [(empty? loc) 0]
        [else (+ 1 (count-list(rest loc)))]))

;; helper function
 (define (check-empties h)
  (cond [(empty? h) 0]
        [else (+ 1 (check-empties(rest h)))]))

(define (valid-hand? h)
  (cond
  [(and (cons? h) (cons? (first(rest h)))
        (equal? (count-list h) 2)
        (equal? (count-list (first(rest h)))2)
        (equal? (count-list (first h))2)
        (not (member? empty (first h)))
        (>= 2 (check-empties h))
        (and (symbol? (first (rest (first h))))
                 (or
                  (symbol=? (first (rest (first h))) 'C) ; or first list's first is one of these letters
                  (symbol=? (first (rest (first h))) 'D)
                  (symbol=? (first (rest (first h))) 'H)
                  (symbol=? (first (rest (first h))) 'S)))
        (and (symbol? (first (rest (first (rest h)))))
                 (or
            (symbol=? (first (rest (first (rest h)))) 'C)
                (symbol=? (first (rest (first (rest h)))) 'D)
                (symbol=? (first (rest (first (rest h)))) 'H)
                (symbol=? (first (rest (first (rest h)))) 'S)))
        (not(and (equal? (first (rest (first h))) (first (rest (first (rest h)))))
                 (equal? (first(first h)) (first(first(rest h))))))

        
        (or (and(integer? (first(first h))) ;; second list's first is an int
                (> (first(first h)) 1) (< (first(first h)) 11))
            (and (symbol? (first (first h)))
                 (or
                  (symbol=? (first(first h)) 'J) ; or first list's first is one of these letters
                  (symbol=? (first(first h)) 'Q)
                  (symbol=? (first(first h)) 'K)
                  (symbol=? (first(first h)) 'A))))
        

        (or (and(integer? (first(first(rest h))))
                (> (first(first(rest h))) 1) (< (first(first(rest h))) 11))
            (and (symbol? (first(first(rest h))))
                 (or
                (symbol=? (first(first(rest h))) 'J)
                (symbol=? (first(first(rest h))) 'Q)
                (symbol=? (first(first(rest h))) 'K)
                (symbol=? (first(first(rest h))) 'A))))) true]
  [else false]))

  

(check-expect (valid-hand? (cons (cons 'J (cons 'H empty))
                                 (cons (cons 10 (cons 'H empty)) empty))) true)
(check-expect (valid-hand? (cons (cons 'A empty)
                                 (cons (cons 10 (cons 'H empty)) empty))) false)
(check-expect (valid-hand? (cons (cons 1 empty)
                                 (cons 5 empty))) false)









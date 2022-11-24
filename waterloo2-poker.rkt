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


;; Problem 2 part a
;; (ordinality) takes in a Card and produces its ordinality according to waterloo2 poker rules

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


;; ordinality: Card -> Nat

;; Tests:
(check-expect (ordinality (cons 'Q(cons 'S empty))) 12)
(check-expect (ordinality (cons 8(cons 'S empty))) 8)
(check-expect (ordinality (cons 'A(cons 'S empty))) 14)
(check-expect (ordinality (cons 'J(cons 'S empty))) 11)
(check-expect (ordinality (cons 10 (cons 'D empty))) 10)

;; Helper function
;; (rank) returns the rank of the card

;; Examples:
(check-expect (rank (cons 3(cons 'S empty))) 3)
(check-expect (rank (cons 'Q(cons 'S empty))) 'Q)
(check-expect (rank (cons 8(cons 'S empty))) 8)

(define (rank card)
  (first card))



;; Problem 2 part b
;; (strength) takes in a hand and outputs its strength accoding to waterloo2 poker rules

;; Examples:
(check-expect(strength (cons (cons 'K (cons 'S empty))
                             (cons (cons 'Q (cons 'D empty)) empty)))2)
(check-expect (strength (cons (cons 3 (cons 'C empty))
                             (cons (cons 'A (cons 'C empty)) empty))) 1)

(define (strength h)
  (cond
    ;;check if it's a straight flush
    [(and (= 1 (abs(-(ordinality(first h)) (ordinality(first(rest h))))))
          (symbol=?(second(first h))(second(second h))))4]
    ;; check if it's straight
    [(= 1 (abs(-(ordinality(first h)) (ordinality(first(rest h))))))2]
    ;;check if it's a flush
    [(symbol=?(second(first h))(second(second h)))1]
    ;; check if it's a pair
    [(= (ordinality(first h)) (ordinality(first(rest h))))3]  
    [else 0]))

;; strength: Hand -> Int

;; Tests:
(check-expect (strength (cons (cons 'J (cons 'H empty))
                             (cons (cons 10 (cons 'H empty)) empty))) 4)
(check-expect (strength (cons (cons 'J (cons 'H empty))
                             (cons (cons 'J (cons 'S empty)) empty))) 3)
(check-expect (strength (cons (cons 2 (cons 'D empty))
                             (cons (cons 5 (cons 'S empty)) empty))) 0)
(check-expect (strength (cons (cons 'J (cons 'D empty))
                             (cons (cons 9 (cons 'D empty)) empty))) 1)


;; Problem 2 part c
;; (hand<?) takes two hands and outputs true if h2 is better than h1.
;; Otherwise it produces false

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

(define (hand<? h1 h2)
  (< (strength h1) (strength h2)))

;; hand<?: Hand Hand -> Sym

;; Tests
(check-expect (hand<?
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty)) ;; flush = 1
              (cons (cons 'J (cons 'S empty))
                    (cons (cons 'Q (cons 'C empty)) empty))) true) ;; straight = 2
(check-expect (hand<?
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 'A (cons 'S empty))
                    (cons (cons 'A (cons 'D empty)) empty))) true)

;; Problem 2 part d
;; (winner) takes two Hands and outputs which hand is better: 'hand1, 'hand2, or 'tie
(check-expect (winner
              (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 'J (cons 'S empty))
                    (cons (cons 'Q (cons 'C empty)) empty))) 'hand2)

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

;; winner: Hand Hand -> Sym 

;; Tests:
(check-expect (winner
              (cons (cons 'J (cons 'H empty))
                    (cons (cons 10 (cons 'H empty)) empty))
              (cons (cons 'A (cons 'S empty))
                    (cons (cons 'A (cons 'D empty)) empty))) 'hand1)
(check-expect (winner
              (cons (cons 'J (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))
              (cons (cons 'J (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))) 'tie)


;; Problem 2 part e
;; (valid-hand?) takes in anything and determines if it's a valid hand,
;; according to waterloo2 poker rules


(define (valid-hand? h)
  (cond
  [(and (cons? h) (cons? (second h))
        (= (count-list h) 2)
        (= (count-list (first(rest h)))2)
        (= (count-list (first h))2)
        (not (member? empty (first h)))
        (>= 2 (check-empties h))
        (and (symbol? (first (rest (first h))))
                 (or
                  (symbol=? (first (rest (first h))) 'C) 
                  (symbol=? (first (rest (first h))) 'D)
                  (symbol=? (first (rest (first h))) 'H)
                  (symbol=? (first (rest (first h))) 'S)))
        (and (symbol? (first (rest (first (rest h)))))
                 (or
            (symbol=? (first (rest (first (rest h)))) 'C)
                (symbol=? (first (rest (first (rest h)))) 'D)
                (symbol=? (first (rest (first (rest h)))) 'H)
                (symbol=? (first (rest (first (rest h)))) 'S)))
        (not(and
             (equal? (first (rest (first h))) (first (rest (first (rest h)))))
                 (equal? (first(first h)) (first(first(rest h))))))
        

        
        (or (and(integer? (first(first h))) 
                (> (first(first h)) 1) (< (first(first h)) 11))
            (and (symbol? (first (first h)))
                 (or
                  (symbol=? (first(first h)) 'J) 
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

;; helper function to count elements in a list
(define (count-list loc)
  (cond [(empty? loc) 0]
        [else (+ 1 (count-list(rest loc)))]))

;; helper function to check if the list contains empty elements
 (define (check-empties h)
  (cond [(empty? h) 0]
        [else (+ 1 (check-empties(rest h)))]))

;; valid-hand?: Any -> Bool
  
;; Tests:
(check-expect (valid-hand? (cons (cons 'J (cons 'H empty))
                                 (cons (cons 10 (cons 'H empty)) empty))) true)
(check-expect (valid-hand? (cons (cons 'A empty)
                                 (cons (cons 10 (cons 'H empty)) empty))) false)
(check-expect (valid-hand? (cons (cons 1 empty)
                                 (cons 5 empty))) false)
(check-expect (valid-hand? (cons (cons 7 (cons 1 empty))
                                 (cons (cons 'A (cons "P" empty)) empty))) false)
(check-expect (valid-hand? (cons (cons 15 (cons 'S empty))
                                 (cons (cons 5 (cons 'A empty)) empty))) false)
(check-expect (valid-hand? (cons 3 (cons 'H empty))) false)
(check-expect (valid-hand? (cons (cons 10 (cons 'D empty))
                                 (cons (cons 10 (cons 'D empty)) empty))) false)
(check-expect (valid-hand? (cons (cons 3 (cons 'H empty))
                    (cons (cons 'J (cons 'H empty)) empty))) true)
(check-expect (valid-hand? (cons (cons 'J (cons 'S empty))
                    (cons (cons 'Q (cons 'C empty)) empty))) true)


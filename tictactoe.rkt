;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 05, Problem 3
;; ***************************************************
;;

(define my_grid1
(list (list 'X '_ '_)
(list '_ 'X 'O)
(list '_ '_ '_)))

(define my_grid2
(list
(list 'O 'X 'X '_ 'O)
(list 'X 'X 'O '_ '_)
(list '_ 'X '_ '_ '_)
(list '_ '_ 'O '_ '_)
(list '_ 'O '_ '_ '_)))

;; Part A
;; (whose-turn grid) finds out whose turn it is

;; Examples:
(check-expect (whose-turn (list (list '_ 'X 'O)
                                (list 'X 'O 'O)
                                (list '_ 'X 'X))) 'O)
(check-expect (whose-turn (list (list '_ 'X 'O)
                                (list 'O 'O '_)
                                (list '_ 'X 'X))) 'X)

;; whose-turn: T3Grid -> (anyof 'X 'O)

(define (whose-turn grid)
  (cond 
  [(= (total-count 'X grid) (total-count 'O grid)) 'X]
  [else 'O]))

;; Tests:
(check-expect (whose-turn (list (list '_ 'X 'O)
                                (list 'X 'O 'O)
                                (list 'X 'O 'X))) 'X)
(check-expect (whose-turn (list (list '_ '_ 'O)
                                (list '_ '_ '_)
                                (list '_ 'X 'X))) 'O)

;; (total-count) is a a helper function that counts the total
;; number of 'X and 'O in a grid

;; Examples:
(check-expect (total-count 'X (list (list '_ 'X 'O)
                                (list 'X 'O 'O)
                                (list '_ 'X 'X)))4)

(check-expect (total-count 'X (list (list 'X 'X 'O)
                                (list 'X 'O 'O)
                                (list '_ 'O '_)))3)

;; total-count: (anyof 'X 'O) T3Grid -> Int

(define (total-count member grid)
  (cond
    [(empty? grid) 0]
    [else (+ (count-members member (first grid))
             (total-count member (rest grid)))]))



;; count-members is a helper function that count the
;; number of a certain symbol in a list of symbols

;; Examples
(check-expect (count-members 'X (list '_ 'X 'O)) 1)
(check-expect (count-members 'X (list 'X 'X 'O)) 2)

;; count-members: (anyof 'X 'O) (listof sym) -> Int

(define (count-members player los)
  (cond
    [(empty? los) 0]
    [(symbol=? player (first los))
     ( + 1 (count-members player (rest los)))]
    [else (count-members player (rest los))]))


;; Part B
;; (grid-ref grid row column) produces the symbol in that location on the grid

;; Examples:
(check-expect (grid-ref my_grid2 0 0) 'O)
(check-expect (grid-ref my_grid1 1 0) '_)


;; grid-ref: T3Grid Nat Nat -> (anyof '_ 'O 'X)

(define (grid-ref grid row col)
  (list-ref (list-ref grid row) col))

;; Tests:
(check-expect (grid-ref my_grid2 0 0) 'O)
(check-expect (grid-ref (list
(list 'O 'O 'X '_ 'O)
(list 'X '_ 'O '_ '_)
(list '_ 'X '_ 'X '_)
(list '_ 'O 'O '_ '_)
(list '_ 'O '_ 'X '_)) 3 3) '_)

(check-expect (grid-ref (list
(list 'O 'O 'X)
(list 'O '_ 'X)
(list 'X 'O 'O)) 2 1) 'O) 


;; Part C
;; (get-column grid n) converts a column into a list of symbols in that column

;; Examples:
(check-expect (get-column my_grid2 2) (list 'X 'O '_ 'O '_))
(check-expect (get-column my_grid1 1) (list '_ 'X '_ ))


;; get-column: T3Grid Nat -> (listof (anyof 'X 'O '_))

(define (get-column grid col)
  ;;recursive through each line and add 
  (cond
    [(empty? grid) empty]
    [else (cons (list-ref (first grid) col) (get-column (rest grid) col))]))

;; Tests:
(check-expect (get-column my_grid2 1) (list 'X 'X 'X '_ 'O))
(check-expect (get-column my_grid2 4) (list 'O '_ '_ '_ '_))


;; Part D
;; (will-win? grid row column player) checks if the player would win by placing it in the
;; given location

;; Examples:
(check-expect (will-win? my_grid2 2 2 'X) false)
(check-expect (will-win? my_grid1 2 2 'O) false)

;; will-win?: T3Grid Nat Nat (anyof 'O 'X) -> Bool

(define (will-win? grid row col player)
  (cond
    [(or (symbol=? 'X (grid-ref grid row col))
         (symbol=? 'O (grid-ref grid row col)))false] 
    [(or (= (count-members player (list-ref grid row))
            (- (list-length (list-ref grid row)) 1))
         (= (count-members player (get-column grid col))
            (- (list-length (list-ref grid col)) 1))) true] 
    [else false])) 


;; Tests:
(check-expect (will-win? my_grid2 3 0 'X) false)
(check-expect (will-win? (list (list 'X 'X '_)
                               (list 'O 'X 'O)
                               (list 'O '_ '_)) 0 2 'X) true) 


;; (list-length) is a helper function that calculates the length of a list

;; Examples:
(check-expect (list-length (list 'X 'O 'O '_)) 4)
(check-expect (list-length (list 'O '_ 'X '_ '_)) 5)

;; list-length: (anyof 'X 'O) -> Int

(define (list-length lst)
  (cond
    [(empty? lst)0]
    [else (+ 1 (list-length (rest lst)))])) 






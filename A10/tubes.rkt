;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct game (tubesize maxcolours tubes))

(define game1-start
  (make-game 2 5
             (list (list 'blue 'blue)
                   (list 'blue 'red))))

(define game1-next
  (make-game 2 2
             (list (list 'blue 'red))))

(define game2-start
  (make-game 4 4
             (list (list 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red))))

(define game2-next
  (make-game 4 0 '()))

(define finished-game
  (make-game 2 5
             (list (list 'blue 'blue)
                   (list 'red 'red))))

(define wrong-game
  (make-game 1 1 (list (list 'blue 'blue)
                   (list 'red 'red))))

(define empty-round
  (make-game 0 0 (list '())))

;; Part A
;; (check-colour? size num los) produces true if both conditions are true: first, every
;; symbol in the list, los, appears size times. Second, there are a maximum of (num) different symbols

;; Examples:
(check-expect (check-colour? 2 5 empty) true)
(check-expect (check-colour? 9 0 empty) true)
(check-expect (check-colour? 1 1 '(bye hi)) false)
(check-expect (check-colour? 1 2 '(bye hi)) true)

;; check-colour?: Nat Nat (listof Sym) -> Bool

(define (check-colour? size num los)
  (local [(define (valid? los count prev num-colour)
            (cond
              [(and (empty? prev) (empty? los)) true]
              [(empty? los) (and (= count size) (<= num-colour num))]
              [(empty? prev) (valid? (rest los) 1 (first los) 1)]
              [(or (> count size) (> num-colour num)) false]
              [(symbol=? prev (first los))
               (valid? (rest los) (add1 count) prev num-colour)]
              [else (valid? (rest los) 1 (first los) (add1 num-colour))]))]
    (valid? (sort los (lambda (x y)
                              (string<? (symbol->string x)
                                        (symbol->string y))))
            0 empty 0)))

;; Tests:
(check-expect (check-colour? 10 10 '(a a b b)) false)
(check-expect (check-colour? 0 0 empty) true) 
(check-expect (check-colour? 1 10 '(a b c d e)) true)


;; Part B
;; (valid-game? gm) evaluates if a Game is valid by checking if it satisifed all conditions such as maximum
;; tube size and maximum colours (based on game rules)

;; Examples:
(check-expect (valid-game? game2-next) true)
(check-expect (valid-game? game2-start) true)
(check-expect (valid-game? wrong-game) false)


;; valid-game?: Game -> Bool

(define (valid-game? gm)
  (local
    [(define (check-tubes tubes)
       (cond
         [(empty? tubes) true]
         [(> (length (first tubes))
             (game-tubesize gm))
          false]
         [else (check-tubes (rest tubes))]))
     (define (flatten tubes)
       (foldl (lambda (lst lsts)
                (append lsts lst)) empty tubes))]
    (and (check-colour? (game-tubesize gm)
                        (game-maxcolours gm)
                        (flatten (game-tubes gm)))
         (check-tubes (game-tubes gm)))))

;; Tests:
(check-expect (valid-game? empty-round) true)
(check-expect (valid-game? finished-game) true)


;; Part C
;; (remove-completed gm) takes in a Game and outputs that game but with completed tubes removed,
;; which is any tube filled with same color balls.

;; Examples:
(check-expect (remove-completed game2-start) game2-next)
(check-expect (remove-completed game2-next) game2-next)

;; remove-completed: Game -> Game

(define (remove-completed gm)
  (local
    [(define (make-gm tubes)
       (make-game (game-tubesize gm)
                  (count-colours tubes 0 empty)
                  tubes))
     
     (define (count-colours tubes count colours)
       (cond
         [(empty? tubes) count]
         [(empty? (first tubes)) (count-colours (rest tubes) count colours)]
         [(member (first (first tubes)) colours)
          (count-colours (cons (rest (first tubes)) (rest tubes)) count colours)]
         [else (count-colours (cons (rest (first tubes)) (rest tubes))
                              (add1 count) (cons (first (first tubes)) colours))]))
     
     (define (rm-complete tubes)
       (filter (lambda (tube)
                 (not (complete? tube 0 empty))) tubes))
     
     (define (complete? tube size prev)
       (cond
         [(empty? tube) (= size (game-tubesize gm))]
         [(empty? prev) (complete? (rest tube) (add1 size) (first tube))]
         [else (and (symbol=? (first tube) prev)
                    (complete? (rest tube) (add1 size) prev))]))]
    (make-gm (rm-complete (game-tubes gm)))))


;; Tests:
(check-expect (remove-completed game1-start) game1-next)
(check-expect (remove-completed empty-round) (make-game 0 0 '()))

;; Part D
;; (finished-game? gm) takes in a Game and determines if it's finished (either all tubes are
;; empty or each tube is full with balls of the same color).

;; Examples:
(check-expect (finished-game? game2-next) true)
(check-expect (finished-game? finished-game) true)


;; finished-game?: Game -> Bool

(define (finished-game? gm)
  (zero? (game-maxcolours (remove-completed gm))))

;; Tests:
(check-expect (finished-game? game2-start) true)
(check-expect (finished-game? empty-round) true)
(check-expect (finished-game? game1-start) false)


;; Part E
;; (num-blocks llos) consumes a list of symbols and find the total number of blocks in the llos.
;; Examples:
(check-expect (num-blocks (list
                           '(a a b a a)
                           '(b a a a a))) 5)

(check-expect (num-blocks (list
                           '(a a a)
                           '(b a a))) 3)

;; num-blocks: (listof Sym) -> Nat

(define (num-blocks llos)
  (local
    [(define (count-blocks los count prev)
       (cond
         [(empty? los) count]
         [(empty? prev) (count-blocks (rest los) 1 (first los))]
         [(symbol=? prev (first los))
          (count-blocks (rest los) count prev)]
         [else (count-blocks (rest los) (add1 count) (first los))]))]
    (foldl (lambda (los sum)
             (+ (count-blocks los 0 empty) sum))
           0
           llos)))

;; Tests:
(check-expect (num-blocks (list
                           '(a b c d)
                           '(e f g h))) 8)

(check-expect (num-blocks (list
                           '()
                           '())) 0)


;; Part F
;; (equiv-game? gm1 gm2) evaluates if gm1 (game 1) and gm2 (game 2) are equivalent.

;; Examples:
(check-expect (equiv-game? game2-start game2-next) false)
(check-expect (equiv-game? game2-start game2-start) true)

;; equiv-game?: Game Game -> Bool

(define (equiv-game? gm1 gm2)
  (cond
    [(not (= (game-maxcolours gm1)
             (game-maxcolours gm2)))
     false]
    [(not (= (game-tubesize gm1)
             (game-tubesize gm2)))
     false]
    [(not (= (length (game-tubes gm1))
             (length (game-tubes gm2))))
     false]
    [else (empty? (filter (lambda (tube)
                            (not (member tube (game-tubes gm2))))
                          (game-tubes gm1)))]))
;; Tests:
(check-expect (equiv-game? empty-round empty-round) true)
(check-expect (equiv-game? empty-round game2-start) false) 

;; Part G
;; (all-equiv?  log1 log2) takes in two lists of games, log1 and log2 and checks if
;; all games in each list are the same. If so, produce true. Else false.

;; Examples:
(check-expect (all-equiv? (list game2-start game2-next)
                          (list game2-next game2-start)) true)

(check-expect (all-equiv? (list game2-next game2-next)
                          (list game2-next game2-start)) false)

;; all-equiv?: (listof Game) (listof Game) -> Bool

(define (all-equiv? log1 log2)
  (local
    [(define (has-equiv? log game)
       (= 1 (length (filter (lambda (gm)
                              (equiv-game? gm game))
                            log))))
     (define (maps-to? log1 log2)
       (foldl (lambda (game flag)
                (and flag (has-equiv? log2 game)))
              true
              log1))]
  (and (maps-to? log1 log2) (maps-to? log2 log1))))


;; Part H
;; (next-games gm)
(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))
(define (next-games gm) empty)
(define 3-by-3 (make-game 3 3
                          (list '(a b a)
                                '(a b b)
                                '())))


;; Examples:
(check-expect (test-next-games 3-by-3 (list (make-game 3 2
                                                       (list '(b a)
                                                             '(a b b)
                                                             '(a)))
                                            (make-game 3 2
                                                       (list '(a b a)
                                                             '(b b)
                                                             '(a))))) true)


(check-expect (next-games empty-round) empty)
;; next-games: Game -> (listof Games)
;(define (next-games gm)
;  (local
;    [(define (
   

               
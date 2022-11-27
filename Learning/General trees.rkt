;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |General trees|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; learning all of cs 135
;; general arithmetic expression trees

;; This is for binary trees?
(define-struct binode (op left right ))
;; a binary arithmetic expression internal node (BINode)
;; is a (make-binode (anyof '+ '* '/ '-) BinExp BinExp

;; this is for general trees?
(define-struct opnode (op args))
;; an OpNode (operator node) is a
;; make-opnode (anyof '+ '*) (listof AExp))


;; EXAMPLES BEGIN
;; Try to draw the trees.
(define opnode-1 (make-opnode '* (list 3 4 5)))

(define opnode-2 (make-opnode '+ (list (make-opnode '* '(4 2)) 3 (make-opnode '+ '(5 1 2)) 2)))


;; Templates for Arithmetic expression

;; An Arithmetic Expression (AExp) is one of:
;; * Num
;; * OpNode

;; try making the function eval: evaluating a make-opnode
(define (eval exp)
  (cond
    [(number? exp) exp]
    [(opnode? exp) (applying (opnode-op exp) (opnode-args exp))]))

(define (applying op args)
  (cond
    [(and (symbol=? '* op)(empty? args)) 1]
    [(and (symbol=? '+ op)(empty? args)) 0]
    [(symbol=? '+ op) (+ (eval (first args)) (applying op (rest args)))]
    [(symbol=? '* op) (* (eval (first args)) (applying op (rest args)))]))

;; Examples
(check-expect (applying '+ (list 1 2 3 4)) 10)
(check-expect (applying '* (list 1 2 0 4)) 0)
(check-expect (eval 4) 4)
(check-expect (eval opnode-1) 60)
(check-expect (eval (make-opnode '+ (list (make-opnode '* '(3 4))
                                          (make-opnode '* '(2 5))))) 22)


;; an Alternative arithmetic expression (AltAExp) is one of:
;; * a Num
;; * (cons (anyof '+ '*) (list of AltAExp)

(define (eval2 expr)
  (cond
    [(number? expr) expr]
    [(cons? expr) (applying2 (first expr) (rest expr))]))

(define (applying2 op args)
  (cond
    [(and (symbol=? '* op)(empty? args)) 1]
    [(and (symbol=? '+ op)(empty? args)) 0]
    [(symbol=? '+ op) (+ (eval2 (first args)) (applying2 op (rest args)))]
    [(symbol=? '* op) (* (eval2 (first args)) (applying2 op (rest args)))]))

(check-expect (eval2 3) 3)
(check-expect (eval2 '(+ 3 4)) 7)
(check-expect (eval2 '(+ (* 4 2) 3 (+ 5 1 2) 2)) 21)

;; Other uses for general trees
(define-struct gnode (key children))
;; a GT (general tree) is a (make-gnode Nat (listof GT))



















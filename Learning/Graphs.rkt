;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Learning graphs M16
;; Defining a Graph's Node and defining the graph

;; Node is a Sym

;A graph is one of:
;* empty
;* (cons (list v (list w_1 ... w_n)) g)
;where g is a Graph
;v, w_1, ...w_n are Nodes

;; A template for graphs
;(define (graph-template g)
;  (cond
;    [(empty? g) ...]
;    [(cons? g)
;     (... (first (first g)))
;     (... (listof-node-template (second (first g)))
;          ... (graph-template (rest g))... )]))

;; Example graph
(define my-graph
'((A (C D E))
(B (E J))
(C())
(D(F J))
(E (K))
(F (K H))
(H ())
(J (H))
(K ())))

(define (node g) (first (first g)))
(define (adj-nodes g) (second (first g)))

;; Example 1: Count neighbors

(check-expect (count-out-neighbours my-graph) (list 3 2 0 2 1 2 0 1 0))

(define (count-out-neighbours g)
  (cond
    [(empty? g) 0]
    [else (cons (length (second (first g)))
                (count-out-neighbours (rest g)))]))

    

    

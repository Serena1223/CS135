;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Module 6 practice 1 - posn|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; defining variables. Notice where the bracket is or isn't.
;; No bracket before constant name. 
(define somewhere (make-posn 7 2))
(define a 84)

;; defining function notice where the bracket is. Has bracket before func name.
(define (add-together a b)
  (+ a b))

;; Working them together
(check-expect (add-together (posn-x somewhere) (posn-y somewhere)) 9) 

;; EX 3 - 2 values for posn? to produce true and two for false
(check-expect (posn? 3) false) ;; false
(check-expect (posn? (posn-x somewhere)) false) ;; false
(check-expect (posn? somewhere) true)
(check-expect (posn? (make-posn 5 5)) true)

;; writing the template for Posn
(define (my-posn-template p)
  (... (posn-x p)...
   ... (posn-y p)...))


;; practice writing a function for posn addition. 
(define (vector-addition p1 p2)
  (make-posn 
  (+(posn-x p1) (posn-x p2))
  (+(posn-y p1) (posn-y p2))))

(check-expect (vector-addition (make-posn 4 5) (make-posn 6 9)) (make-posn 10 14))
  
;; EX. 6
(define-struct book (title author year))
;; a Book is a (make-book Str Str Nat)

(define favbook (make-book "harry potter" "jk rowling" 2000))
(check-expect (book-author favbook) "jk rowling")

(define (add-year b adl)
  (make-book (book-title b) (book-author b) (+ adl(book-year b))))

(check-expect (add-year favbook 2) (make-book "harry potter" "jk rowling" 2002))





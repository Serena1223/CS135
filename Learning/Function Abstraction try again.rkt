;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Function Abstraction try again|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Learning M16 (Try to do 7 questions for now)
;; Main ideas: Abastraction

;; Example 1:
;; Using lambda and filter. Consumes a (listof Str) and
;; produces a list containing all strings that have a length of 4

(define (keep4 lst)
  (filter (lambda (str) (= 4 (length (string->list str)))) lst))

(check-expect (keep4 '("hell" "nope" "hi")) (list "hell" "nope"))

;;--------By the way-----------;;
;; Original:
;; (define (interest-earned amount) (* interest-rate amount))

;; Lambda version
;; (lambda (amount) (* interest-rate amount))
;;--------------------------;;

;; Example 2:
;; Using map

(define (squash-range lst)
  (map (lambda (num) (/ num 255)) lst))

(check-expect (squash-range ' (0 204 255)) '(0 0.8 1))

;; Example 3:
;; use map to greet each person
(define (greet-each los)
  (map (lambda (name) (string-append "Hi " name)) los))

(check-expect (greet-each '("james" "carlos")) (list "Hi james" "Hi carlos"))

;; Example 4:
;; make odd numbers negative and leave even numbers positive
(define (neg-odd lon)
  (map (lambda (num) (cond
                       [(odd? num) (- num)]
                       [else num])) lon))

(check-expect (neg-odd (list 3 5 6 9 10)) (list -3 -5 6 -9 10))

;; Example 5:
;; count number of odd integers in a list

;; map and foldr: map even into 0 and odd into 1
(define (count-odd1 lon)
  (foldr + 0 (map (lambda (num) (cond
                              [(odd? num) 1]
                              [else 0])) lon)))

(check-expect (count-odd1 '(9 10 11 12)) 2)

;; using foldr with filter
;; Using filter to get a list that has only odd stuff
;; then use foldr to sum up how many elements are in the list
(define (count-odd2 lon)
  (foldr (lambda (x rror) (+ 1 rror)) 0 (filter odd? lon)))

(check-expect (count-odd2 '(9 10 11 12)) 2)

;; using foldr only
;; the lambda function adds one if it's odd and adds 0 if it's even
(define (count-odd3 lon)
  (foldr (lambda (x rror) (cond
                            [(odd? x) (+ 1 rror)]
                            [else rror])) 0 lon))

(check-expect (count-odd3 '(9 10 11 12)) 2)


;; Example 6:
;; use foldr to write a prod that produces the product of list of num

(define (prod lon)
  (foldr (lambda (x rror) (* x rror)) 1 lon))

(check-expect (prod '(2 2 3 5)) 60)

;; Example 7:
;; total-length of elements in a list
;; version 1: using length
(define (total-lengthv1 lst)
  (cond
    [(empty? lst) 0]
    [(cons? (first lst)) (+ (length (first lst))
                            (total-lengthv1 (rest lst)))]
    [else (+ 1 (total-lengthv1 (rest lst)))]))

(check-expect (total-lengthv1 '((1 2 3) (4 5) (1 1 1))) 8)

;; version 2: using foldr
;; the f function here figures out the length of each element
(define (total-lengthv2 lst)
  (foldr (lambda (x rror) (+ (length x) rror)) 0 lst))

(check-expect (total-lengthv2 '((1 2 3) (4 5) (1 1 1))) 8)


;; Example 8: foldr to produce mean of a list of num
(define (average lon)
  (foldr f 0 lon))







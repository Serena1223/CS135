;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Functions as values|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; M14 Functions as values

;; Consuming Functions
;; consume another function as an argument

;; The function filter
(define (myfilter pred? lst)
  (cond
    [(empty? lst) empty]
    [(pred? (first lst)) (cons (first lst) (myfilter pred? (rest lst)))]
    [else (myfilter pred? (rest lst))]))
    

;; Example 1: filter only multiples of 3
(check-expect (keep-multiples3 (list 1 2 3 4 5 6 7 8 9 10)) (list 3 6 9))

(define (keep-multiples3 lst)
  (local [(define (multiple-of-3? item) (= (remainder item 3) 0))]
    (myfilter multiple-of-3? lst)))

;; Example 2: filters the multiples of 2 OR 3
(check-expect (keep-multiples23 (list 1 2 3 4 5 6 7 8 9 10)) (list 2 3 4 6 8 9 10))

(define (keep-multiples23 lst)
  (local [(define (multiple-2or3? item)
            (or (even? item)(= 0 (remainder item 3))))]
    (myfilter multiple-2or3? lst)))

;; Example 3: keep-in-range only keeps values between 10 and 30
(check-expect (keep-in-range (list -5 10.1 12 7 30 3 19 6.5 42))
              (list 10.1 12 30 19))

(define (keep-in-range lst)
  (local [(define (within30? item) (and (<= item 30) (>= item 10)))]
    (myfilter within30? lst)))

;; Example 5
(define (sum-odds-or-evens lst)
  (local [(define (sumup lst)
            (cond
              [(empty? lst) 0]
              [else (+ 1 (sumup (rest lst)))]))]
  (cond
    [(> (sumup (myfilter odd? lst)) (sumup (myfilter even? lst)))
     (sumup (myfilter odd? lst))]
    [else (sumup (myfilter even? lst))])))

(check-expect (sum-odds-or-evens (list 4 5 6 1 2 3 0 0 0 0 0 0)) 9)


;; Make adder
(define (make-adder n)
  (local
    [(define (f m) (+ m n))]
    f))

;; Example 6: Write a function (make-divisible? n) that produces a predicate function. The
;; predicate function consumes a Int, returns true if its argument is divisible by n, and
;; false otherwise.

(define (make-divisible? n)
  (local
    [(define (f m) (= 0 (remainder m n)))]
    f))

(check-expect (filter (make-divisible? 2) (list 0 1 2 3 4 5 6 7 8 9))
(list 0 2 4 6 8))
(check-expect (filter (make-divisible? 3) (list 0 1 2 3 45 67 8 9))
(list 0 3 45 9))
(check-expect (filter (make-divisible? 4) (list 0 1 23 4 5 67 8 9))
(list 0 4 8))

;; Binding functions to identifiers (bound identifier)

;; Current
((make-adder 2) 3) ;; will give 5

;; New idea
(define add2 (make-adder 2)) ;; now you can use add2 over and over









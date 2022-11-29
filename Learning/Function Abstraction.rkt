;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Function Abstraction|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; M16 Function Abstraction
;; (lambda (x_1 ... x_n) exp)

(define (eat-apples lst)
  (filter (lambda (item) (not (symbol=? item 'apple))) lst))


;;(define (make-between n m)
 ;; (local [(define (f x) (and (<= n x) (<= x m)))] f))

(define (make-between n m)
  (lambda (x) (and (<= n x) (<= x m))))

(filter (make-between 2 4) '(0 1 2 3 4 5 6))

(define (bar lon)
(foldr max (first lon) (rest lon)))
(bar ' (1 5 23 3 99 2)) 

(define (foo los)
  (foldr (lambda (s rror) (+ (string-length s) rror)) 0 los))

(foo '("one" "two" "three"))

(define (negate-list lst)
  (cond [(empty? lst) empty]
        [else (cons (- (first lst))
                    (negate-list (rest lst)))]))

;; negate-list using foldr
(lambda (x rror) (cons (- x) rror))

(define (negate-list lst)
  (foldr (lambda (x rror) (cons (- x) rror)) empty lst)) ;; cuz it takes 2 elements: x and rror

;; my-map using foldr
(define (my-map f lst)
  (foldr (lambda (x rror) (cons (f x) rror)) empty lst))

;; use foldr to maintain original order. 




















;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam review M12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Finals preparation
;; M12 - Mutual recursion

;; When functions apply each other.

;;
(define (keep-alternates lst)
  (cond [(empty? lst) empty]
        [else (cons (first lst) (drop-alternates (rest lst)))]))

(define (drop-alternates lst)
  (cond [(empty? lst) empty]
        [else (keep-alternates (rest lst))]))

(keep-alternates (list 1 2 3 4 5 6 7 9))

;; "determine if natural number n is odd or even"

(define (check-even/odd n)
  (even n))

(define (even n)
  (cond [(= n 0) "even"]
        [else (odd (- n 1))]))

(define (odd n)
  (cond [(= n 0) "odd"]
        [else (even (- n 1))]))

(check-expect (check-even/odd 9) "odd")
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tryinga63a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Part B
;; (intersection s1 s2) produces the overlaps between set 1 and 2
;; Examples:
(check-expect (intersection (list 1 3 4) (list 1 5)) (list 1))
(check-expect (intersection (list 1 6 9) (list 1 3 9)) (list 1 9))

;; intersection: (setof Nat) (setof Nat) -> (setof Nat)

(define (intersection s1 s2)
  (cond
    [(empty? s1) empty]
    [(empty? s2) empty]
    [(= (first s1) (first s2)) (cons (first s1) (intersection (rest s1) (rest s2)))]
    [(< (first s1) (first s2)) (intersection (rest s1) s2)]
    [else (intersection s1 (rest s2))]))

;; Tests:
(check-expect (intersection (list 1 3 9 11 30) (list 1 9 30)) (list 1 9 30))
(check-expect (intersection (list -1 3 10 15) (list 1 3 10)) (list 3 10))
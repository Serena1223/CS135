;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname range) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 04, Problem 2
;; ***************************************************
;;

;; Part A
;; (in-range a b listofnum) checks the number of elements in
;;a list that are between two numbers a and b, inclusive

;; Examples
(check-expect (in-range 5 9
                        (cons 3 (cons 9 (cons 0 (cons 1 (cons 2 empty)))))) 1)
(check-expect (in-range 0 3
                        (cons 3.1 (cons 0.0 (cons 2 (cons 9 empty))))) 2)

;; in-range: Num Num (Listof num) -> Nat

(define (in-range a b listofnum)
  (cond
    [(empty? listofnum) 0]
    [(or (and
          (<= a b)
          (>= (first listofnum) a)
          (<= (first listofnum) b))
         
         (and
          (>= a b)
          (<= (first listofnum) a)
          (>= (first listofnum) b))) (+ 1 (in-range a b (rest listofnum)))]
    [else (in-range a b (rest listofnum))]))

;; Tests
(check-expect (in-range -1 4 (cons 3 (cons pi (cons 1 (cons -2 empty))))) 3)
(check-expect (in-range 3 3 (cons 3 (cons 0 (cons -2 empty)))) 1)
(check-expect (in-range 7 1 (cons 2 (cons 2 (cons -2 (cons 7 empty))))) 3)


;; Part B
;; (spread lon) sorts a list of numbers and ouputs the positive
;; difference between the max and min numbers in the list

;; Examples
(check-expect (spread (cons 100 (cons 0 (cons -1 empty)))) 101)
(check-expect (spread (cons -1 (cons -10 empty))) 9)

;; spread: (listof Num) -> Nat

(define (spread lon)
  (abs(- (first(sorts lon)) (last-element (sorts lon)) )))


;; Tests (for the actual function)
(check-expect (spread (cons 2 (cons 2 (cons 2 empty)))) 0)
(check-expect (spread (cons 190 (cons -10 (cons 3 empty)))) 200)
(check-expect (spread (cons 0 (cons -1 empty))) 1)



;; (sorts lon) is a helper function that sorts from least to greatest

;; Examples
(check-expect (sorts (cons 4 (cons 1 (cons 8 empty))))
              (cons 1 (cons 4 (cons 8 empty))))
(check-expect (sorts (cons 1 (cons 0 (cons -1 empty))))
              (cons -1 (cons 0 (cons 1 empty))))

;;sorts: (listof Num) -> (listof Num)

(define (sorts lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon) (sorts (rest lon)))]))



;; (insert n slon) is a helper function that places n before the next element 
;; of the list that's greater or equal to itself

;; Examples
(check-expect (insert 4 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
              (cons 2 (cons 3 (cons 4 (cons 4 (cons 5 empty))))))
(check-expect (insert 6 (cons 4 (cons 6 (cons 1 empty))))
              (cons 4 (cons 6 (cons 6 (cons 1 empty)))))
              
;; insert: Num (listof Num) -> (listof Num)

(define (insert n slon) 
  (cond [(empty? slon) (cons n empty)] 
        [(<= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))



;; (last-element l) is a helper function that retrieves the last item in a list

;; last-element: (listof Any) -> Any

;; Examples
(check-expect (last-element (cons 1 (cons 4 (cons 9 empty)))) 9)
(check-expect (last-element (cons 0 (cons 6 empty))) 6)

;; last-element: (listof Any) -> Any

(define (last-element l)
  (cond
    [(empty? (rest l)) (first l)]
    [else(last-element(rest l))]))




              


             
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


;; Example 7:
;; total-length of elements in a list
;; version 1: using length

(define (total-lengthv1 lst)
  (cond
    [(empty? lst) 0]
    [(cons? (first lst)) (+ (length (first lst)) (total-lengthv1 (rest lst)))]))

(check-expect (total-lengthv1 '((1 2 3) (4 5) (1 1 1))) 8)

(define (total-lengthv2 lst)
  (foldr (lambda (item rror) (+ (length item) rror)) 0 lst))

(check-expect (total-lengthv2 '((1 2 3) (4 5) (1 1 1))) 8)

;; took a break. back in waterloo now

;; Example 8 - mean of non-empty (listof Num)
(define (average lst)
  (/ (foldr (lambda (item rror) (+ item rror)) 0 lst) (length lst)))
(check-expect (average '(2 4 9)) 5)


;; Example 9 - times-square
(define (times-square lon)
  ;; produce a list of perf squares using filter
  (foldr * 1 (filter (lambda (item) (integer? (sqrt item))) lon)))
  
(check-expect (times-square '(1 25 5 4 1 17)) 100)

;; Using CONS with foldr
;; negate list using foldr
(lambda (x rror) (cons (- x) rror))

;; becomes
(define (negatelist lst)
  (foldr (lambda (x rror) (cons (- x) rror)) empty lst))

;; my map using foldr
(define (foldr-map func lst)
  (foldr (lambda (x rror) (cons (func x) rror)) empty lst))

;; my filter using foldr
(define (foldr-filter func lst)
  (foldr append empty (lambda (x rror) (func x))))

;; Example 10 - double each
(define (double-each lst)
  (foldr (lambda (x rror) (cons (* 2 x) rror)) empty lst))

(check-expect (double-each '(3 5 9)) '(6 10 18))

;; Example 11 - kee-evens list

(define (keep-evens lst)
  ;; the function will
  ;; it'll be (foldr f b lst) and the f is going to do the filter stuff
  (foldr (lambda (item filtered) (cond
                                   [(even? item) (cons item filtered)]
                                   [else filtered])) empty lst))
(check-expect (keep-evens '(2 3 4 5 6 9)) '(2 4 6))


;; Example 12 - sum even
;; first version
(define (sum-even1 loi)
  (foldr cons empty (filter even? loi)))

(check-expect (sum-even1 '(2 3 5 6 7 8)) '(2 6 8))

;; second version - lambda instead of even
(define (sum-even2 loi)
  (foldr cons empty (filter (lambda (item) (= 0 (remainder item 2))) loi)))

(check-expect (sum-even2 '(2 3 5 6 7 8)) '(2 6 8))

;; third version - foldr, lambda, even? but no filter
(define (sum-even3 loi)
  (foldr (lambda (x filtered) (cond
                                [(even? x) (cons x filtered)]
                                [else filtered])) empty loi))

(check-expect (sum-even3 '(2 3 5 6 7 8)) '(2 6 8))

;; Example 13 - TIME TO DEBUG THIS
(define (multiple-each lst n)
  (map (lambda (item) (* item n)) lst))

(check-expect (multiple-each '(1 3 3 4) 2) '(2 6 6 8))

;; Example 14 - add-total
(define (add-total lst)
  (map (lambda (item) (+ item (foldr + 0 lst))) lst))

(check-expect (add-total (list 1 9 1 -1)) (list 11 19 11 9))

;; Example 15 - (discord-bad)
(define (discard-bad lst lo hi)
  (filter (lambda (item) (and (<= lo item) (>= hi item))) lst))

(check-expect (discard-bad '(12 5 20 2 10 22) 10 20) '(12 20 10))


;; Example 16 - squash-bad
(define (squash-bad lo hi lst)
  (map (lambda (item) (cond
                        [(< item lo) lo ]
                        [(> item hi) hi]
                        [else item])) lst))

(check-expect (squash-bad 10 20 '(12 5 20 2 10 22)) '(12 10 20 10 10 20))

;; Example 17 - above-average
(define (above-average lon)
  (filter (lambda (item)(< (/ (foldr + 0 lon) (length lon)) item)) lon))

(above-average (list -10 2 9 1 20 19))
 







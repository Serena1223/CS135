;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tryinga62e) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Part e
;; (sort-evs)
(define-struct ev (model year price mileage mpge))
;; Examples:
(check-expect (sort-evs (list (make-ev "Alpha Tauri" 1999 1000 34353 10101)
                              (make-ev "Mercedes Lewis" 2013 1523524 25 53535)
                              (make-ev "F1 Car" 2020 53000 34131 231)
                              (make-ev "Red Bull" 1909 1251251 34153 66342)))
                        (list (make-ev "F1 Car" 2020 53000 34131 231)
                              (make-ev "Mercedes Lewis" 2013 1523524 25 53535)
                              (make-ev "Alpha Tauri" 1999 1000 34353 10101)
                              (make-ev "Red Bull" 1909 1251251 34153 66342)))

(define (compare-ev ev1 ev2)
  (cond
    [(> (ev-year ev1) (ev-year ev2)) 'gt]
    [(< (ev-year ev1) (ev-year ev2)) 'lt]
    [(< (ev-mpge ev1) (ev-mpge ev2)) 'lt]
    [(> (ev-mpge ev1) (ev-mpge ev2)) 'gt]
    [(< (ev-mileage ev1) (ev-mileage ev2)) 'gt]
    [(> (ev-mileage ev1) (ev-mileage ev2)) 'lt]
    [else 'eq]))

;; sort-evs: (listof EV) -> (listof EV)



(define (find-best-ev/acc loev best-so-far new-list)
  (cond
    [(empty? loev) (cons best-so-far new-list)]
    [(symbol=? (compare-ev (first loev) best-so-far) 'gt)
     (find-best-ev/acc (rest loev) (first loev) (cons best-so-far new-list))]
    [else (find-best-ev/acc (rest loev) best-so-far (cons (first loev) new-list))]))

(define (find-best-ev loev)
  (find-best-ev/acc loev (first loev) empty))


(define (sort-evs/sf loev)
  (cond
    [(empty? (rest loev)) (cons (first loev) empty)]
    [else (cons (first loev) (sort-evs/sf (find-best-ev(rest loev))))]))

(define (sort-evs loev)
  (sort-evs/sf (find-best-ev loev)))


;; Examples:
(check-expect (compare-ev (make-ev "bcar" 1999 101010 9000 1000)
                          (make-ev "hi car" 2021 2 100 34343)) 'lt)
(check-expect (compare-ev (make-ev "Merc car" 2023 101010 9000 1000) 
                          (make-ev "Red bull" 2023 200 100 34343)) 'lt)



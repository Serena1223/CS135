;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname testingheap) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "heap-support.rkt")
(define my-heap (make-hnode 1
                            (make-hnode 4
                                        (make-hnode 9 '() '())
                                        '())
                            (make-hnode 6
                                        (make-hnode 11 '() '())
                                        '())))

;; Part E
;; Examples:
(check-expect (heap-sort (list 4 5 1 3 5) <=) (list 1 3 4 5 5))
(check-expect (heap-sort (list -4 -1 -2 -3) <=) (list -4 -3 -2 -1))
(check-expect (heap-sort (list -4 -9 2 0) <=) (list -9 -4 0 2))
(check-expect (heap-sort empty <=) empty)

(define (heap-sort lst comp?)
  (hea
  


  



  





    





  
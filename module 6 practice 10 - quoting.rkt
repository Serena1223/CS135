;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |module 6 practice 10 - quoting|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
'(4 "donkey" ice-cream) ;; me trying. Final iteration. 
(cons 4 (cons "donkey" (cons 'ice-cream empty)))


;; me trying
'(paper pen "eraser" (32 pencil ("calculator")))

;; supposed answer
(list 'paper 'pen "eraser" (list 32 'pencil (list "calculator")))


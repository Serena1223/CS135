;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sarcasm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 04, Problem 4
;; ***************************************************
;;

;; Part A
(define (pair-listof-X-template lox)
  (cond
    [(empty? lox) ...] ;;check if there's no element
    [(empty? (rest lox)) (... (first lox))]
    [else (...(first lox) ...(second lox)
          (pair-listof-X-template (rest(rest lox))))]))

;; pair-listof-X-template: (pair-listof X) -> Any


;; Part B
;; (sarcastic lox) outputs a string in sarcasm casing, that is,
;; odd letters upper cased and even letters lower cased

;; Examples
(check-expect (sarcastic "Hello buddy") "HeLlO BuDdY")
(check-expect (sarcastic "good LUCK") "GoOd lUcK")

;; pair-listof-X-template: Str -> Str
   
(define (sarcastic s)
  (list->string
  (sarcastic-content (string->list s))))

;; Tests
(check-expect (sarcastic "fun") "FuN")
(check-expect (sarcastic " Eeeeee") " eEeEeE")
(check-expect (sarcastic "you got this") "YoU GoT ThIs")



;; (sarcastic-content lox) is a helper function that operates
;; on a list of characters to upper
;; and lower case them following sarcasm semantics

;; Examples:
(check-expect (sarcastic-content (list #\h #\e #\l))
              (list #\H #\e #\L))
(check-expect (sarcastic-content (list #\A #\L #\space #\H))
              (list #\A #\l #\space #\h))
(check-expect (sarcastic-content (list #\O #\B))(list #\O #\b))

;; sarcastic-content: (listof Char) -> (listof Char)

(define (sarcastic-content lox)
  (cond
    [(empty? lox) empty]
    [(empty? (rest lox)) (cons (char-upcase (first lox)) empty)]
    [else (cons (char-upcase (first lox))
                (cons (char-downcase(second lox))
                      (sarcastic-content (rest(rest lox)))))]))





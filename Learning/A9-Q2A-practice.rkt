;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname A9-Q2A-practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Part A
;; alphanumeric-only takes in a list of strings and only keeps the elements with
;; alphanumerics only

;; Examples:
(check-expect (alphanumeric-only '("efew9ge01" "+%^dhelo" "*?")) '("efew9ge01"))
(check-expect (alphanumeric-only '("efew9ge01" "90" "+%^?" "*")) '("efew9ge01" "90"))
(check-expect (alphanumeric-only '()) '())

;; alphanumeric-only: (listof String) -> (listof String)

(define (alphanumeric-only lst)
  (filter (lambda (str)
            (foldr (lambda (c rror) (and (or (char-alphabetic? c) (char-numeric? c))
                                         rror)) true (string->list str))) lst))
                           
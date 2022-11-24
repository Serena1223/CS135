;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Serena Ge (20998413)
;; CS 135 Fall 2022
;; Assignment 05, Problem 2
;; ***************************************************
;;

(define my_IL_example
  (list (list "water" (list "b.txt"))
        (list "bat" (list "a.txt" "c.txt"))
        (list "bases" (list "c.txt"))
        (list "bot" (list "b.txt" "c.txt"))
        (list "beauty" (list "a.txt"))
        (list "iu" (list "c.txt"))
        (list "he" (list "a.txt" "b.txt" "c.txt"))))

;; Part A
;; (both dl1 dl2) compares if there are documents that occer in both DLs and
;; returns the matches in a list
 
;; Examples:
(check-expect (both (list "a.txt" "b.txt") (list "b.txt" "c.txt")) (list "b.txt"))
(check-expect (both (list "b.txt" "c.txt" "d.txt")
                    (list "c.txt" "d.txt")) (list "c.txt" "d.txt"))

;; both: DL DL -> (listof Str)

(define (both dl1 dl2)
  (cond
    [(empty? dl1) empty]
    [(empty? dl2) empty]
    ;;already includes the case when dl1 and dl2 are both empty
    [(check-member (first dl1) dl2)
     (cons (first dl1) (both (rest dl1) dl2))]
    [else (both (rest dl1) dl2)]))
  
;; Tests:
(check-expect (both (list "a.txt" "b.txt" "c.txt") empty) empty)
(check-expect (both (list "b.txt" "d.txt" "e.txt")
                    (list "b.txt"  "d.txt" "f.txt")) (list "b.txt"  "d.txt"))
(check-expect (both (list "b.txt")
                    (list "a.txt" "b.txt" "d.txt" "f.txt")) (list "b.txt"))


;; check-member is a helper function that checks if a string is a part of a list of strings

;; Examples:
(check-expect (check-member "hi" (list "hi" "bye")) true)
(check-expect (check-member "hi" (list "i" "" "bye")) false)
(check-expect (check-member "hi" (list "  " "hei" "hi")) true)

;; check-member: Str (listof Str) -> Bool ;; CHECK list of STR
(define (check-member word los)
  (cond
    [(empty? los) false]
    [(string=? word (first los)) true]
    [else (check-member word (rest los))]))


;; Part B
;; (exclude dl1 dl2) compares two DLs and outputs a list of
;; documents that only occer in the first DL

;; Examples:
(check-expect (exclude (list "d.txt" "c.txt") (list "d.txt")) (list "c.txt"))
(check-expect (exclude (list "a.txt") empty) (list "a.txt"))

;; exclude: DL DL -> (listof Str)

(define (exclude dl1 dl2)
  (cond
    [(empty? dl1) empty]
    [(empty? dl2) dl1]
    [(not (check-member (first dl1) dl2))
     (cons (first dl1)(exclude (rest dl1) dl2))] ;; not a member of dl2
    [else (exclude (rest dl1) dl2)]))

;; Tests:
(check-expect (exclude (list "a.txt" "c.txt" "d.txt") (list "d.txt")) (list "a.txt" "c.txt"))
(check-expect (exclude (list "a.txt" "c.txt" "f.txt")
              (list "a.txt" "c.txt" "f.txt" "g.txt")) empty)
(check-expect (exclude (list "") (list "")) empty) 


;; Part C
;; (keys-retrieve doc an-il) checks if doc (a string) is contained in an-ils 
;; and returns a list of members in its key-value pair

;; Examples:
(check-expect (keys-retrieve "a.txt" my_IL_example )
              (list "bat" "beauty" "he"))

(check-expect (keys-retrieve "z.txt" my_IL_example) empty)

;; keys-retrieve: Str IL -> (listof Str)

(define (keys-retrieve file-name IL)
  (cond
    [(empty? IL) empty]
    [(check-member file-name (second(first IL))) ;; if list part contains file-name
     (cons (first(first IL)) (keys-retrieve file-name (rest IL)))] 
    [else (keys-retrieve file-name (rest IL))]))

;; Tests:
(check-expect (keys-retrieve "c.txt" my_IL_example)
              (list "bat" "bases" "bot" "iu" "he"))
(check-expect (keys-retrieve "opop.txt" my_IL_example) empty)
(check-expect (keys-retrieve "b.txt" my_IL_example) (list "water" "bot" "he"))

;; Part D
;; (search has 2 cases)
;; (search 'both str1 str2 an-il):  Outputs a list of documents from an-il that
;; contains both str1 and str2

;; (search 'exclude str1 str2 an-il): Outputs a list of documents from an-il that only
;; contains str1

;; Examples:
(check-expect (search 'both "iu" "bot" my_IL_example) (list "c.txt"))
(check-expect (search 'exclude "bot" "water" my_IL_example) (list "c.txt"))

;; search: Syn Str Str IL -> DL

(define (search operation str1 str2 IL)
  (cond
    [(empty? IL) empty]
    [(symbol=? operation 'both)
     (both (find-contents str1 IL) (find-contents str2 IL))]
    [(symbol=? operation 'exclude) (exclude (find-contents str1 IL) (find-contents str2 IL))]
    [else "idk"])) 

;; Tests:
(check-expect (search 'both "water" "bot" my_IL_example) (list "b.txt"))
(check-expect (search 'both "bat" "he" my_IL_example) (list "a.txt" "c.txt"))
(check-expect (search 'exclude "bat" "beauty" my_IL_example) (list "c.txt"))
(check-expect (search 'exclude "he" "water" my_IL_example) (list "a.txt" "c.txt"))


;; find-contents is a helper function that checks which txt files contain a string

;; Examples:
(check-expect (find-contents "cat" IL_example) (list "a.txt" "c.txt"))
(check-expect (find-contents "dog" IL_example) (list "b.txt" "c.txt"))
(check-expect (find-contents "barks" IL_example) (list "b.txt"))

;; find-contents: Str IL -> Str

(define (find-contents str IL)
  (cond
    [(empty? IL) empty]
    [(string=? str (first(first IL))) (second(first IL))]
    [else (find-contents str (rest IL))]))



;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname learning-bt-practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (key left right)) ;; legit definition

(define ex-tree (make-node 5
                           (make-node 1
                                        (make-node 3 empty empty)
                                        (make-node 6 empty empty))
                           (make-node 1
                                      (make-node 1
                                                 (make-node 4 empty empty)
                                                 (make-node 6 empty empty))
                                      (make-node 3 empty empty))))


;; Module: this is supposed to count the nodes in a tree
(define (count-nodes tree k)
  (cond
    [(empty? tree) 0]
    [(node? tree)
     (+ (cond [(= k (node-key tree)) 1]
              [else 0])
        (count-nodes (node-left tree) k)
        (count-nodes (node-right tree) k))]))

(check-expect (count-nodes ex-tree 1) 3)

;; Example 1: Count-leaves t
(define (count-leaves tree)
  (cond
    [(empty? tree) 0]
    [(node? tree)
     (+ (cond
          [(and (empty? (node-left tree))
                (empty? (node-right tree))) 1]
          [else 0])
     (count-leaves (node-left tree))
     (count-leaves (node-right tree)))]))
(check-expect (count-leaves ex-tree) 5)

;; Example 2: Count-even t
(define (count-even t)
  (cond
    [(empty? t) 0]
    [(node? t)
     (+(cond
         [(= (remainder (node-key t) 2) 0) 1]
         [else 0])
     (count-even (node-left t))
     (count-even (node-right t)))]))
(check-expect (count-even ex-tree) 3)

;; Example 3: Reverse-tree t
(define (reverse-tree t)
  (cond
    [(empty? t) 0]
    [(node? t) (make-node (node-key t) 
               (reverse-tree (node-right t))
               (reverse-tree (node-left t)))]))
(reverse-tree ex-tree) ;; passed

;; Module example 2. Just makes sense. Next
(define (search-bt k tree)
  (cond
    [(empty? tree) false]
    [else (or (= k (node-key tree))
              (search-bt k (node-left tree))
              (search-bt k (node-right tree)))]))

;; Module example. Find path to key. Works but inefficient. 
(define (search-bt-path tree k)
  (cond
    [(empty? tree) false]
    [(= k (node-key tree)) '()]
    [(list? (search-bt-path k (node-left tree)))
     (cons 'left (search-bt-path k (node-left tree)))]
    [(list? (search-bt-path k (node-right tree)))
     (cons 'right (search-bt-path k (node-right tree)))]
    [else false]))


;; Improved binary search path

;; Example 4: insert-path (this is a great exercise).
(define (insert-path tree path val)
  (cond [(empty? path) (make-node val (node-left tree)
                                  (node-right tree))]
        [(symbol=? (first path) 'left) (make-node (node-key tree)
                                                  (insert-path (node-left tree) (rest path) val)
                                                  (node-right tree))]
        [(symbol=? (first path) 'right) (make-node (node-key tree)
                                                   (node-left tree)
                                                   (insert-path (node-right tree) (rest path) val))]))
;;(insert-path ex-tree '(left right) 99) ;; works!!! 


;; Example 5: add path (not sure how it works. Not working)
(define (add-path tree path value)
  (cond [(empty? path) (make-node value (node-left tree)
                                  (node-right tree))]
        [(symbol=? (first path) 'left) (make-node (node-key tree)
                                                  (add-path (node-left tree) (rest path) value))]
        [(symbol=? (first path) 'right) (make-node (node-key tree)
                                                   (add-path (node-right tree) (rest path) value))]))
(add-path ex-tree '(left right) 890)
        
;;;;;;;;; BST TIME ;;;;;;;;;;
;; search-bst: ez win.
(define (search-bst n t)
  (cond [(empty? t) false]
        [(= n (node-key t)) true]
        [(< n (node-key t)) (search-bst n (node-left t))]
        [(> n (node-key t)) (search-bst n (node-right t))]))

;; adding to a BST
;; add new key

(define (add-to-bst key tree)
  (cond [(empty? tree) (make-node key empty empty)]
        [(and (node? tree) (= (node-key tree) key))
         tree]
        [(< key (node-key tree))
         (make-node (node-key tree) key empty)] ;; then add to left?
        [(> key (node-key tree))
         (make-node (node-key tree) empty key)])) ;; dk what's going on here.

;; example binary tree

(define bst-tree (make-node 5
                            (make-node 1 (make-node 0 empty empty)
                                         (make-node 3 empty empty))
                            (make-node 6 (make-node 14 empty empty) empty))) 

(add-to-bst 8 bst-tree)

        
        


     

        
          
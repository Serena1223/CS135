;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam review M16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Finals preparation
;; M11 - Binary trees

;; Write the binary tree node data definition
;; - could be a node or empty
(define-struct node (key left right))
;; (make-node BT BT)

;; Here's an example tree:
(define first-tree (make-node 5
                               (make-node 1 empty empty)
                               (make-node 7
                                          (make-node 6 empty empty)
                                          (make-node 14 empty empty))))

;; Here's a template for binary tree:
;; Has two conditions of whether the tree is empty. In the case it's not empty
;; do something to the node's key and the left sub tree and right sub tree
(define (bt-template t)
  (cond [(empty? t) ...]
        [(node? t) (... (node-key t) ;; this is the key part of functionality
                    (bt-template (node-left t))
                    (bt-template (node-right t)))]))

;; Make a sum keys
(define (sum-keys tree)
  (cond
    [(empty? tree) 0]
    [(node? tree) (+ (node-key tree)
                     (sum-keys (node-left tree))
                     (sum-keys (node-right tree)))]))



;; Make a function that increments the key
(define (incr t)
  (cond [(empty? t) empty]
        [(node? t) (make-node (+ 1 node-key) ;; this is the key part of functionality
                    (incr node-left t)
                    (incr node-right t))]))



;; Ex. 1: count-leaf nodes
(define (leaf t)
  (cond [(empty? t) 0]
        [(node? t) (+ (cond
                        [(and (empty? (node-left t))
                              (empty? (node-right t))) 1]
                        [else 0]);; this is the key part of functionality
                    (leaf (node-left t))
                    (leaf (node-right t)))]))

(check-expect (leaf first-tree) 3) 

; very noiceee

;; Ex 2: produce the number of nodes with an even key
(define (even-key t)
  (cond [(empty? t) 0]
        [(node? t) (+ (cond [(even? (node-key t)) 1]
                         [else 0]);; this is the key part of functionality
                    (even-key (node-left t))
                    (even-key (node-right t)))]))

(check-expect (even-key first-tree) 2) 

; too ezzzzz


;; Ex 3: reverse tree (swap left with right, vice versa)
(define (reverso t)
  (cond [(empty? t) empty]
        [(node? t) (make-node (node-key t) ;; this is the key part of functionality
                    (reverso (node-right t))
                    (reverso (node-left t)))]))

(reverso first-tree) ; yasss toooo ezzzz


;; we are entering: searching binary trees. Including finding path to key and stuff
(define (search-bt-path k tree)
  (cond
    [ (empty? tree) false]
    [ (= k (node-key tree)) '()]
    [(list? (search-bt-path k (node-left tree)) )
     (cons
      'left (search-bt-path k (node-left tree))) ]
    [(list? (search-bt-path k (node-right tree) ))
     (cons
      'right (search-bt-path k (node-right tree) )) ]
    [else false]))

;; Ex 4: insert-path
;(define (insert-path t path val)
;  (cond [(empty? t) empty]
;        [(node? t) (... (node-key t) ;; this is the key part of functionality
;                    (insert-path (node-left t))
;                    (insert-path (node-right t)))]))

;; Do this again
(define (insert-path tree path value)
  (cond
    [(empty? tree) empty] ; Return empty if the tree is empty
    [(empty? path) (make-node value (node-left tree) (node-right tree))] ; Return the tree with the new value if the path is empty
    [(symbol=? (first path) 'left)
     (make-node (node-key tree) ; Return the tree with the left subtree modified
                (insert-path (node-left tree) (rest path) value)
                (node-right tree))]
    [(symbol=? (first path) 'right)
     (make-node (node-key tree) ; Return the tree with the right subtree modified
                (node-left tree)
                (insert-path (node-right tree) (rest path) value))]
    [else empty])) ; Return empty if the path is invalid

(insert-path first-tree '(right right) 200) 

;; Ex 5 WRITE IT LATER!! COME BACK (DO REMEMBER)
(define (add-path tree path value)
  (cond
    [(empty? tree) empty]
    [empty? path]))

;; Making a BST
(define aBST (make-node 5
           (make-node 3 empty empty)
           (make-node 7 (make-node 6 empty empty) (make-node 14 empty empty))))

;; Ex 6: we are finally using BST. Ok so note that when making BST stuff,
;; a lot of conds are necessary it seems like
(define (count-smaller n t)
  (cond
    [(empty? t) 0]
    [(< n (node-key t)) (+ 1 (count-smaller n (node-left t))
                            (count-smaller n (node-right t)))]
    [(>= n (node-key t)) (count-smaller n (node-left t))]
    [else 0]))

(count-smaller 5 aBST)



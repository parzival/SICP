; Section 2.2.2

(define (header phrase)
  (newline)
  (display phrase)
  (newline)
  )

; Needed for Racket/PB
(define nil null)


; Ex 2.24
; Give the interpreter result, box-and-pointer, and tree representations

; displaying interpreter result
(displayln "Result of expression:")
(list 1 (list 2 (list 3 4)))

; Ex 2.25
; Give the combination of cars and cdrs that picks 7 from the list

(define a-list (list 1 3 (list 5 7) 9))
(define b-list (list(list 7)))
(define c-list (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; Getting result
(header "Picking 7 from a list")

(define (equal-7? expr)
  (display 
   (if (number? expr)
       (if (= expr 7)
           "correct"
           expr
           )
      expr
      )
   )
  (newline)
  )

;(equal-7? <expression with a-list>)
;(equal-7? <expression with b-list>)
;(equal-7? <expression with c-list>)

; Ex 2.26
; List operations

(define x (list 1 2 3))
(define y (list 4 5 6))

(header "Operations on two lists")
(append x y) ; Combine the two lists 
(cons x y) ; x is the first item of the pair, which becomes a list since y is a list
(list x y) ; a list with two items in it, that are also lists

; Ex 2.27
; Deep (recursive) reverse

; Modify reverse from 2.18 to also reverse any elements that are lists

; Replace with definition of reverse from 2.18. 
(define (reverse) () )
              
; Testing 
(define x (list (list 1 2) (list 3 4)))

(header "Testing deep-reverse.")
(equal? (list ()) (deep-reverse (list ())))
(equal? (list 1) (deep-reverse (list 1)))
(define single-depth-list (list 1 2 3 4 5 6))
(equal? (list (list 4 3) (list 2 1)) (deep-reverse x))
(equal? (reverse single-depth-list) (deep-reverse single-depth-list))
(reverse x)          ; ((3 4) (1 2))
(deep-reverse x)         ; ((4 3) (2 1))
(deep-reverse c-list)
(deep-reverse (list a-list x (list b-list x)))  

; Ex 2.28
; Showing the fringe of a tree


; Testing
(header "Checking fringe.")
(define x (list (list 1 2) (list 3 4)))
(equal? (list 1 2 3 4) (fringe x))
;(fringe x)            ; (1 2 3 4)
(equal? (list 1 2 3 4 1 2 3 4) (fringe (list x x)))
;(fringe (list x x))   ; (1 2 3 4 1 2 3 4)
(fringe (list ()))    ; empty list
(fringe (list 1))     ; list of just one item
(fringe (list (list 1 2) 3 (list 4 5) (list (list 6 7) (list 8 9))))


; Ex 2.29
; Binary mobiles

(define (make-mobile left right) 
  (list left right)
  )

(define (make-branch length structure) 
  (list length structure)
  )

; a. Write selectors for the branches, length, and structure


; b. Write procedure to find the total weight of the mobile

; Testing total weight
(header "Testing mobile total-weight.")

(define branch-5-1 (make-branch 5 1))
(define mob1 (make-mobile branch-5-1 branch-5-1))

(total-weight mob1) ; 2

(define branch-4-2 (make-branch 4 2))
(define mob2 (make-mobile branch-4-2 (make-branch 3 mob1)))
(total-weight mob2) ; 4

; c. Check if a mobile is balanced.

(define (test-true x) (displayln (if x "passed" "failed")))
(define (test-false x) (displayln (if (not x) "passed" "failed")))

(header "Testing for balanced predicate")
(test-true (<?balanced-p?> mob1)) 
(test-false (<?balanced-p?> mob2)) 

(define mob3 (make-mobile 
              (make-branch 5 (make-mobile (make-branch 3 (make-mobile branch-4-2 branch-4-2))
                                          (make-branch 6 2)
                                          )
                           )
              (make-branch 5 (make-mobile (make-branch 6 mob1) (make-branch 3 4)))
              )
  )
(test-true (<?balanced-p?> mob3)) 

; d.  Changing the constructors ... how much needs fixing?

(header "Changing constructors for mobiles")

(define (show-mobiles mob-list)
  (define (mob-show mob)
    (display "Weight:")
    (display (total-weight mob))
    (display " Balanced?:")
    (display (<?balanced-p?> mob))
    (newline)
    )
  (for-each mob-show mob-list)
  )

; Make some mobiles (using original constructors)
(define branch-5-1 (make-branch 5 1))
(define mob1 (make-mobile branch-5-1 branch-5-1))
(define branch-4-2 (make-branch 4 2))
(define mob2 (make-mobile branch-4-2 (make-branch 3 mob1)))
(define mob3 (make-mobile 
              (make-branch 6 (make-mobile (make-branch 3 (make-mobile branch-4-2 branch-4-2))
                                          (make-branch 6 2)
                                          )
                           )
              (make-branch 6 (make-mobile (make-branch 6 mob1) (make-branch 3 4)))
              )
  )

; Test original construction
(displayln "Using original constructors:")
(show-mobiles (list mob1 mob2 mob3))  
(test-true (balanced? mob1))
(test-false (balanced? mob2))
(test-true (balanced? mob3))

; Change the constructors
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

; What else needs to be changed?


; Make the mobiles using the new constructors
(define branch-5-1 (make-branch 5 1))
(define mob1 (make-mobile branch-5-1 branch-5-1))
(define branch-4-2 (make-branch 4 2))
(define mob2 (make-mobile branch-4-2 (make-branch 3 mob1)))
(define mob3 (make-mobile 
              (make-branch 6 (make-mobile (make-branch 3 (make-mobile branch-4-2 branch-4-2))
                                          (make-branch 6 2)
                                          )
                           )
              (make-branch 6 (make-mobile (make-branch 6 mob1) (make-branch 3 4)))
              )
  )


; Test again
(displayln "Using new constructors:")
(show-mobiles (list mob1 mob2 mob3)) 
(test-true (balanced? mob1))
(test-false (balanced? mob2))
(test-true (balanced? mob3))

; Ex 2.30
; Squaring a tree

; refer to definitions from Ex 2.21 as needed
(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (sqr (car items)) (square-list-1 (cdr items)))
      )
  )

(define (square-list-2 items)
  (map sqr items)
  )

      
; Testing  
(header "Testing square-tree")

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define squared-test-tree (list 1 (list 4 (list 9 16) 25) (list 36 49)))

;(square-tree-1 test-tree)
(equal? squared-test-tree (square-tree-1 test-tree))
;(square-tree-2 test-tree)
(equal? squared-test-tree (square-tree-2 test-tree))

(equal? (square-tree-1 test-tree) (square-tree-2 test-tree))

; Ex 2.31
; Map for trees

(define (tree-map proc tr)
  (map (lambda(subtr)
         (if (pair? subtr)
             (tree-map proc subtr)
             (proc subtr
             )
         )
       )
       tr
       )
  )


; Testing
(header "Testing tree-map version of square-list")

(define (square-tree-3 tr) (tree-map sqr tr))

;(square-tree-3 test-tree)
(equal? squared-test-tree (square-tree-3 test-tree))

; Ex 2.32
; Generating the power set

; Complete the definition, and explain how the function works
(define (subsets s) 
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest))
        )
      )
  )


; Testing
(header "Testing subsets")
(subsets (list 1 2 3))
(subsets (list (list 1 2) 3 4))

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

(equal-7? (car (cdr (car (cdr (cdr a-list))))))
(equal-7? (car (car b-list)))
(equal-7? (car (cdr 
      (car (cdr 
            (car (cdr 
                  (car (cdr 
                        (car (cdr 
                              (car (cdr c-list))
                              )
                             )
                        )
                       )
                  )
                 )
            )
           )
      )
     )
          )

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

(define (reverse li)
  (define (move-list l1 l2)
    (if (null? l1)
       l2
      (move-list (cdr l1) (cons (car l1) l2))
      )
    )
  (move-list li ())
  )

(define (deep-reverse li)
  (define (iter li answer)
    (cond
      ((null? li) answer)
      ((list? (car li)) (iter (cdr li) (cons (deep-reverse (car li)) answer)))
      (else (iter (cdr li) (cons (car li) answer)))
      )
    )
  (iter li ())
  )
                
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

(define (fringe li)
    (cond
      ((null? li) nil)
      ((pair? li) (append (fringe (car li)) (fringe (cdr li))))
      (else (list li))
    )
  )

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

(define (left-branch mob) (car mob))

(define (right-branch mob) (car(cdr mob)))

(define (branch-length br) (car br))

(define (branch-structure br) (car (cdr br)))

; b. Write procedure to find the total weight of the mobile
 
; A few helping functions.
(define (is-branch? b) (list? b))
(define (is-mobile? m)
  (if (list? m)
      (and (is-branch? (left-branch m)) (is-branch? (right-branch m)))
      false
      )
  )

(define (branch-weight br)
  (if (is-mobile? (branch-structure br))
      (total-weight (branch-structure br))
      (branch-structure br)
      )
  )

(define (total-weight mob)
  (+ (branch-weight (left-branch mob)) (branch-weight (right-branch mob)))
  )

; Testing total weight
(header "Testing mobile total-weight.")

(define branch-5-1 (make-branch 5 1))
(define mob1 (make-mobile branch-5-1 branch-5-1))

(total-weight mob1) ; 2

(define branch-4-2 (make-branch 4 2))
(define mob2 (make-mobile branch-4-2 (make-branch 3 mob1)))
(total-weight mob2) ; 4

; c. Check if a mobile is balanced.

(define (balanced? mob)
  (define (moment br)
    (* (branch-weight br) (branch-length br))
    )
  (if (not(is-mobile? mob))
      true
      (and (balanced? (left-branch mob))
           (balanced? (right-branch mob))
           (= (moment (left-branch mob)) (moment (right-branch mob)))
           )
      )
  )

(define (test-true x) (displayln (if x "passed" "failed")))
(define (test-false x) (displayln (if (not x) "passed" "failed")))

(header "Testing for balanced predicate")
(test-true (balanced? mob1)) 
(test-false (balanced? mob2)) 

(define mob3 (make-mobile 
              (make-branch 5 (make-mobile (make-branch 3 (make-mobile branch-4-2 branch-4-2))
                                          (make-branch 6 2)
                                          )
                           )
              (make-branch 5 (make-mobile (make-branch 6 mob1) (make-branch 3 4)))
              )
  )
(test-true (balanced? mob3)) 

; d.  Changing the constructors ... how much needs fixing?

(header "Changing constructors for mobiles")

(define (show-mobiles mob-list)
  (define (mob-show mob)
    (display "Weight:")
    (display (total-weight mob))
    (display " Balanced?:")
    (display (balanced? mob))
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
(define (right-branch mob) (cdr mob))

(define (branch-structure br) (cdr br))

(define (is-branch? b) (pair? b))
(define (is-mobile? m)
  (if (pair? m)
      (and (is-branch? (left-branch m)) (is-branch? (right-branch m)))
      false
      )
  )

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

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (sqr (car items)) (square-list-1 (cdr items)))
      )
  )

(define (square-list-2 items)
  (map sqr items)
  )

(define (square-tree-1 tr)
  (cond
    ((null? tr) nil)
    ((pair? tr) (cons (square-tree-1 (car tr)) (square-tree-1 (cdr tr))))
    (else (sqr tr))
    )
  )

(define (square-tree-2 tr)
  (map (lambda(x)
         (if (pair? x)
             (square-tree-2 x)
             (sqr x)
             )
         )
       tr
       )
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

(displayln "Showing cubed tree")
(define (cube-tree tr) (tree-map (lambda(x) (expt x 3)) tr))
(cube-tree test-tree)

; Ex 2.32
; Generating the power set

; Complete the definition, and explain how the function works
(define (subsets s) 
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda(x) (cons (car s) x)) rest))
        )
      )
  )


; Each subset can be built by adding one element to all the lower subsets. This is what cons does, and by having the added element first, it will build each element as a separate list.  Compare the coin-counting example, which uses a similar process (although that only keeps track of the count).

; Testing
(header "Testing subsets")
(subsets (list 1 2 3))
(subsets (list (list 1 2) 3 4))

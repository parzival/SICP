; Section 2.3.3

; Unordered non-duplicated list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))
        )
  )

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set))
  )

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)
               )
         )
        (else (intersection-set (cdr set1) set2))
        )
  )

; Not included in the text, but useful (and also used in tests)
(define empty-set '()) 

; Verification 
(define (check-true x) (displayln (if x "passed" "failed, expected true")))
(define (check-false x) (displayln (if (not x) "passed" "failed, expected false")))
 

; Construct some fake sets
; These are 'fake' even if they match a set exactly, because they do not use adjoin-set to add members.
(define sample-element 5)
(define sample-element-2 'word)
(define non-element 'non-element)
(define non-empty-set (list sample-element))
(define mixed-set (list sample-element sample-element-2))

(define (element-tests)
  (displayln "Testing (element-of-set?)...")
  (check-false (element-of-set? sample-element empty-set))          ; empty set has no elements
  (check-true  (element-of-set? sample-element non-empty-set))      ; element is correctly found
  (check-false (element-of-set? sample-element-2 non-empty-set))    ; elements not in the set are not found
  (check-true  (element-of-set? sample-element-2 mixed-set))        ; A set can have multiple elements
  (check-true  (element-of-set? sample-element mixed-set))          ; ... and both are found
  (check-false (element-of-set? non-element mixed-set))            ; Non-elements are not found in larger sets
  (displayln "done.")
  )

(element-tests)

(define (check-is-element x set)
  (check-true (element-of-set? x set))
  )
(define (check-is-not-element x set)
  (check-false (element-of-set? x set))
  )

(define (adjoin-tests)
  (displayln "Testing (adjoin-set)")
  (let ((simple-set (adjoin-set sample-element empty-set))
        (two-set (adjoin-set sample-element-2 (adjoin-set sample-element empty-set)))
        )
    (check-is-element sample-element simple-set)                      ; adjoin adds to the set
    (check-is-not-element sample-element-2 simple-set)                ; elements not in the set not added
    (check-is-element sample-element two-set)                         ; larger sets add elements properly
    (check-is-element sample-element-2 two-set)                       ; larger sets add all elements
    (check-is-not-element non-element two-set)                       ; larger sets do not add non-elements   
    (check-true (equal? two-set (adjoin-set sample-element-2 simple-set))) ; Sets can be changed and compared
    (check-false (equal? simple-set two-set))                            ; ensure different sets are different
    (check-true (equal? two-set (adjoin-set sample-element two-set))) ; Adding the same element does not change the set's composition
    )
  (displayln "done.")
  )
; Note the use of equal? to test for equality (implicitly assuming the sets to work as lists)

(adjoin-tests)

; Create a set from a list
(define (build-set li)
  (if (null? li)
      empty-set
      (adjoin-set (car li) (build-set (cdr li)))
      )
  )

; Convert a set to a list
(define (set->list set)
  set
  )

; Ex 2.59.
; Implementing union-set for unordered lists

(define (union-set set1 set2)
  (if (null? set1) 
      set2
      (adjoin-set (car set1) (union-set (cdr set1) set2))
      )
  )

; Testing

; A better definition of equal for sets:
; Two sets are equal if every element in each one is also in the other set.

(define (sets-equal? a b)
  ; andmap - like flatmap, just using Boolean AND to accumulate
  (and (andmap (lambda (x) (element-of-set? x b)) (set->list a)) 
       (andmap (lambda (x) (element-of-set? x a)) (set->list b))
       )
  )

; Set a is the item to test; b is expected value
(define (check-sets-equal a b)
  (if (sets-equal? a b)
      (displayln "passed")
      (displayln a)
      )
  )

(define (set-op-tests) 
  (displayln "Testing set operations (intersection and union)...")
  (let ((test-set1 (build-set '(a b c)))
        (test-set2 (build-set '(d b e f)))
        (test-set3 (build-set '(g i h g c)))
        )
    (check-sets-equal (intersection-set test-set1 test-set2)   (build-set '(b)))
    (check-sets-equal (intersection-set test-set3 test-set3)   (build-set '(c g h i)))
    (check-sets-equal (union-set test-set1 test-set2)          (build-set '(a b c d e f)))
    (check-sets-equal (union-set test-set1 test-set3)          (build-set '(a b c g h i)))
    (check-sets-equal (union-set test-set1 empty-set)          (build-set '(a b c))) 
    (check-sets-equal (union-set empty-set test-set1)          test-set1)
    (check-sets-equal (union-set test-set2 empty-set)          test-set2)
    (check-sets-equal (intersection-set empty-set test-set1)   empty-set)
    (check-sets-equal (intersection-set test-set2 empty-set)   empty-set)
    )
   (displayln "done.")
  )

(set-op-tests)

; Ex 2.60.
; Allowing duplicate elements

; First, an amendment to our tests, using sets-equal?:
(define (duplicate-element-tests)
  (displayln "Testing sets with duplicate elements...")
  (let ((simple-set (build-set '(a b c d)))
        (dup-set1 (build-set '(a b a c)))
        (dup-set2 (build-set '(c e f g c e f g c)))
        )
    (check-is-element 'a dup-set1)                      ; adjoin adds the duplicate to the set
    (check-is-element 'c dup-set1)                      ; non-duplicate elements are added
    (check-sets-equal simple-set (adjoin-set 'd dup-set1)) ; Duplicate-element sets are equal 
    (check-sets-equal (union-set dup-set1 dup-set2) (build-set '(a b c e f g)))  ; test union of sets with duped elements
    (check-sets-equal (intersection-set simple-set dup-set1) (build-set '(a b c))) ; test intersection of sets with duped elements
    )
  (displayln "done.")
  )

(duplicate-element-tests) ; passes when duplicate elements are ignored

; element-of-set? remains the same

(define (adjoin-set x set)
  (cons x set)
  )

; intersection-set remains the same

(define (union-set set1 set2)
  (append set1 set2)
  ) 

; Comment on efficiency and usefulness.

; The non-duplicating set is obviously much better in terms of space.  This also probably
; leads to better performance in time when using (element-of-set?), as the speed of the search
; depends on the size of the list used to store the set.  The main advantage the 
; duplicate-allowed version has is that adjoin is a much faster function.  In a situation where
; elements are frequently added (and the speed of this operation is important) but infrequently
; accessed, the second version would likely be better.  In pretty much all other circumstances,
; the non-duplicate version performs better.

; Testing

; Note that sets-equal? is unchanged.  Sets with a differing number of duplicate elements are considered equal by our definition.

(newline)
(displayln "Testing allowing duplicate elements")
(element-tests)
(adjoin-tests) ; Last test in this series will fail (it tests internal composition, not set equality)
(set-op-tests) ; These tests should pass
(duplicate-element-tests)

; 2.61
; Ordered list adjoin-set

; Defined in text
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))
        )
  )

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))
                     )
               )
              ((< x1 x2)
               (intersection-set (cdr set1) set2)
               )
              ((< x2 x1)
               (intersection-set set1 (cdr set2))
               )
              )
        )
      )
  )

;(element-tests) ; still using old fake sets, will have an error

; New fake sets, to use elements orderable with <, >
(define sample-element 5)
(define sample-element-2 7)
(define non-element -5)
(define non-empty-set (list sample-element))
(define mixed-set (list sample-element sample-element-2))

(newline)
(displayln "Testing ordered sets")
(element-tests)

; define adjoin-set to work faster than with unordered sets (on average)

; The number of calls to adjoin-set (and comparisons with x) are O(n) on
; average, but compared to the unordered version the function returns 
; once the position for the element is found.

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((el (car set)))
        (cond
          ((> x el) (cons el (adjoin-set x (cdr set))))
          ((< x el) (cons x set))
          (else set); element is = x
          ) 
        )
      )
  )

(adjoin-tests)

; Observing internal ordering
(displayln "Observing ordering of ordered sets")
(define test-set1 (build-set '(1 2 3)))
(define test-set2 (build-set '(4 2 5 6)))
(define test-set3 (build-set '(7 9 8 7 3)))

test-set1  ; {1 2 3}
test-set2  ; {2 4 5 6}
test-set3  ; {3 7 8 9}

; Ex 2.62
; Ordered-list union-set in linear time

; Give a Theta(n) implementation of union-set for ordered list 

(define (union-set set1 set2)
  (cond 
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((x1 (car set1)) (x2 (car set2)))
       (cond ((= x1 x2)
              (cons x1 (union-set (cdr set1) (cdr set2)))
              )
             ((< x1 x2)
              (cons x1 (union-set (cdr set1) set2))
              )
             ((< x2 x1)
              (cons x2 (union-set set1 (cdr set2)))
              )
             )
       )
     )
    )
  )

; Must switch to using numbers instead of symbols
(define (set-op-tests)
  (displayln "Testing set operations (intersection and union)...")
  (let ( (test-set1 (build-set '(1 2 3)))
         (test-set2 (build-set '(4 2 5 6)))
         (test-set3 (build-set '(7 9 8 7 3)))
         )
    (check-sets-equal (intersection-set test-set1 test-set2)   (build-set '(2)))
    (check-sets-equal (intersection-set test-set3 test-set3)   (build-set '(3 7 8 9)))
    (check-sets-equal (union-set test-set1 test-set2)          (build-set '(1 2 3 4 5 6)))
    (check-sets-equal (union-set test-set1 test-set3)          (build-set '(1 2 3 7 8 9)))
    (check-sets-equal (union-set test-set1 empty-set)          (build-set '(1 2 3))) 
    (check-sets-equal (union-set empty-set test-set1)          test-set1)
    (check-sets-equal (union-set test-set1 empty-set)          test-set1)
    (check-sets-equal (intersection-set test-set1 empty-set)   empty-set)
    (check-sets-equal (intersection-set empty-set test-set2)   empty-set)       
    )
  (displayln "done.")
  )

(set-op-tests)

; Preserve these versions

(define (intersection-oset set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-oset (cdr set1)
                                       (cdr set2))
                     )
               )
              ((< x1 x2)
               (intersection-oset (cdr set1) set2)
               )
              ((< x2 x1)
               (intersection-oset set1 (cdr set2))
               )
              )
        )
      )
  )

(define (union-oset set1 set2)
  (cond 
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((x1 (car set1)) (x2 (car set2)))
       (cond ((= x1 x2)
              (cons x1 (union-oset (cdr set1) (cdr set2)))
              )
             ((< x1 x2)
              (cons x1 (union-oset (cdr set1) set2))
              )
             ((< x2 x1)
              (cons x2 (union-oset set1 (cdr set2)))
              )
             )
       )
     )
    )
  )


; Ex 2.63
; Comparing two methods of tree conversion

; Tree set-up (definitions from text)
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
(define (make-tree entry left right)
  (list entry left right)
  )


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree)))
              )
      )
  )

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list
                                          )
                            )
                      )
        )
    )
  (copy-to-list tree '())
  )
; a. Do the two tree->list procedures produce the same result for the same trees? If not, how do they differ?
 
; The first procedure recursively moves down the left branch (without adding entries) until it 
; reaches a leaf.  As it backs out it will add first the root of the branch, and then move down
; the right branch until another left branch is found.   Entries are thus added in the order left
; branch, center, right-branch by appending the center and right branch to the left branch's entries.

 
; The second procedure builds the list in a slightly opposite direction; first the right branch is
; traversed until the right-most leaf is found.  At this point the right leaf is added to the 
; result-list.  As it backs up the tree, each root entry is added to the head of the result-list,
; which gets passed to the left branch.  Once the left-most branch is found, it will 'jump' up
; to the root of that branch.  In the end, the entries will end up in the same order - left branches
; at the head, then the root entry, then the right branches.

; What lists result from the trees given in the figure?

; Testing
(define (make-leaf x) (make-tree x '() '())) ; for convenience

; These produce the same result for the trees in the book's figure.
(define tree-1 (make-tree 7 (make-tree 3 (make-leaf 1) (make-leaf 5)) (make-tree 9 '() (make-leaf 11))) )
(define tree-2 (make-tree 3 (make-leaf 1) (make-tree 7 (make-leaf 5) (make-tree 9 '() (make-leaf 11)))) )
(define tree-3 (make-tree 5 (make-tree 3 (make-leaf 1) '()) (make-tree 9 (make-leaf 7) (make-leaf 11))) )

(define fig-trees (list tree-1 tree-2 tree-3))

(define (test-trees)
  (for-each (lambda(tr) 
              (display (tree->list-1 tr)) 
              (display (tree->list-2 tr))
              (display " Equal-check:")
              (check-sets-equal (build-set (tree->list-1 tr)) (build-set (tree->list-2 tr)))
              (newline)
              )
            fig-trees
            )
  )

(displayln "Checking-tree-to-list methods")
(test-trees)


; b. Do the procedures have the same order of growth in the number of steps to
; convert a balanced tree? Which is faster if they are not equal?
; 
; The first procedure must make an append operation at each node.  In a balanced
; tree, it will append half the tree below it at each depth.  Since each node
; has 2 nodes below it in the tree, half the entire tree will be appended at
; each depth (At d = 0, n/2; at d = 1, 2 * n/4).
; Assume append(n) takes an O(n) number of steps (most likely).  Then there
; will be 1/2*(nlog(n)) total steps due to append since the tree depth is log n,
; and n steps for the cons operation.
; That makes this procedure O(n*(1 + log(n))).
; The second procedure operates on each node only once, using cons, making
; it O(n). It will grow in number of steps more slowly than the first.



; Ex. 2.64.
; Converting a list to a tree

(define (list->tree elements)
  (car (partial-tree elements (length elements)))
  )

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1)))
                )
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size
                                              )
                                )
                  )
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result))
                    )
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts
                      )
                )
              )
            )
          )
        )
      )
  )

; a. Describe how partial-tree works.



; Testing

(displayln "Observing (list->tree)")
(list->tree '())
(list->tree '(1))
(list->tree (list 1 2 3))
(list->tree '(1 3 5 7 9 11))
(list->tree '(1 3 4 5))
(list->tree '(5 3 4 1)) ; invalid input -- list is not ordered

; b. What is the order of growth in steps required?

; The procedure is O(n). Each element of the list is processed only once, 
; and once placed, it is kept in its proper position and not accessed again.  

(define tree->list tree->list-2) ; select one to use, for clarity

; Ex. 2.65.
; Implementing union-set and interstection-set on binary trees

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)
                          )
         )
        ((> x (entry set))
         (element-of-set? x (right-branch set)
                          )
         )
        )
  )

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)
                    )
         )
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))
                    )
         )
        )
  )

; Testing
; Replace some definitions
(define (set->list set) (tree->list set))
(define (build-set li) (list->tree (sort li <)))  ; sort is in Racket, and sorts using the second argument

; The faked sets need to be converted to trees
(define non-empty-set (list->tree non-empty-set))
(define mixed-set (list->tree mixed-set))

(newline)
(displayln "Running set tests using tree implementation")
(element-tests)
(adjoin-tests)

; Simplest way by using previous exercises - convert the tree to a list
; Work the linear op on the list, and then convert back to a tree. All
; operations are O(n).

(define (union-set tset1 tset2)
  (list->tree (union-oset (tree->list tset1) (tree->list tset2)))
  )

(define (intersection-set tset1 tset2)
  (list->tree (intersection-oset (tree->list tset1) (tree->list tset2)))
  )

; Testing
(set-op-tests)

(displayln "Observing tree set operations")
(define tree-4 (make-tree 15 (make-tree 4 (make-leaf 2) (make-leaf 12)) (make-tree 20 (make-leaf 18) (make-leaf 24))) )
(define tree-5 (make-tree 8 (make-leaf 7) (make-leaf 12)))
(define tree-6 (make-tree 8 (make-tree 7 '() '()) (make-tree 12 '() '())))

(union-set tree-1 tree-2)  ; { 1 3 5 7 9 11}
(intersection-set tree-1 tree-2) ; { 1 3 5 7 9 11}
(union-set tree-5 '())    ; { 8 7 12}
(union-set tree-5 tree-1) ; { 1 3 5 7 8 9 11 12 }
(intersection-set tree-5 tree-1) ; { 7 }
(intersection-set tree-4 tree-1) ; { }
(union-set tree-4 tree-3) ; { 1 2 3 4 5 7 9 11 12 15 18 20 24 }

; Ex. 2.66.
; Implementing key-based lookup in trees

(define (lookup-uolist given-key set-of-records) 
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records))) (car set-of-records))
        (else (lookup-uolist given-key (cdr set-of-records)))
        )
  )

; This is more or less identical to 'element-of-set?' above
(define (lookup-tree given-key set-of-records)
  (if (null? set-of-records) false
      (let ((x given-key)
            (record-key (key (entry set-of-records)))
            )
        (cond 
          ((equal? x record-key)
           (value (entry set-of-records))
           )
          ((< x record-key)
           (lookup-tree x (left-branch set-of-records))
           )
          ((> x record-key)
           (lookup-tree x (right-branch set-of-records))
           )
          )
        )
      )
  )


; Testing

(displayln "Observing key-based lookup in trees")

; Using letters as records; the key is the position in the alphabet.
(define (key ltr) (add1 (- (char->integer ltr) (char->integer #\a))))
(define (value entry) entry)

(define alpha-list (list #\d #\o #\r #\l #\t #\x #\b))

(define alpha-tree (list->tree alpha-list))

(lookup-uolist 12 alpha-list) ; "l"
(lookup-uolist 14 alpha-list) ; false

(lookup-tree 12 alpha-tree) ; "l"
(lookup-tree 14 alpha-tree) ; false

; Modifying it to use key-value pairs
(define (key entry) (car entry))
(define (value entry) (cdr entry))

(define name-list (list (cons 19 "T. Leighton")
                        (cons 3 "H. Sato")
                        (cons 7 "E. Molson")
                        (cons 23 "K. Riley")
                        (cons 40 "J. Kirk" )
                        )
  )

(define name-tree (list->tree name-list))

(lookup-uolist 7 name-list) ; returns whole record (7 . "E. Molson")
(lookup-uolist 13 name-list) ; false
(lookup-tree 7 name-tree)  ; "E. Molson"
(lookup-tree 13 name-tree) ; false





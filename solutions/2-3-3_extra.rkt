(load "2-3-3_sol.scm")  ; loads the previous file; rename as necessary
(displayln "File loaded.")

; Creates a list of n items, from 0 to n-1
(define (list-of-n n) 
  (define (build-iter i built)
    (if (>= i 0) 
        (build-iter (sub1 i) (cons i built))
        built
        )
    )
  (build-iter (sub1 n) '())
  )

; Takes the first n of a list (reversed, but we don't care for our uses)
(define (first-n-of-list li n)
  (define (build-iter orig built n)
    (if (<= n 0)
        built
        (build-iter (cdr orig) (cons (car orig) built) (sub1 n))
        )
    )
  (build-iter li '() n)
  )

; The unordered versions, from the text (renamed)
(define (element-of-uoset? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-uoset? x (cdr set)))
        )
  )

(define (adjoin-uoset x set)
  (if (element-of-uoset? x set)
      set
      (cons x set))
  )


(define (intersection-uoset set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-uoset? (car set1) set2)
         (cons (car set1)
               (intersection-uoset (cdr set1) set2)
               )
         )
        (else (intersection-uoset (cdr set1) set2))
        )
  )

; include a definition for union-uoset; use exercise 2.59

; building sets

; This version works for both, but we need to have them in parallel.
;
;(define (build-set li)
;  (if (null? li)
;      empty-set
;      (adjoin-set (car li) (build-set (cdr li)))
;      )
;  )

(define (build-uoset li)
  li
  )

; That gives us a chance to optimize
(define (build-oset li)
  (sort li <)
  )

; also rename ordered versions as necessary.

; From solutions file
(define (adjoin-oset x set)
  (if (null? set)
      (list x)
      (let ((el (car set)))
        (cond
          ((> x el) (cons el (adjoin-oset x (cdr set))))
          ((< x el) (cons x set))
          (else set); element is = x
          ) 
        )
      )
  )

(define (element-of-oset? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-oset? x (cdr set)))
        )
  )


; Testing makes use of  random lists

; Randomize a list 
(define (randomize-list-n start-list n)
  (define (rand-iter i base rlist)
    (if (> i 0) 
        (let ((x (list-ref base (random i))))
          (rand-iter (sub1 i)
                     (remove x base)
                     (cons x rlist)
                     )
          )
        rlist
        )
    )
  (rand-iter (length start-list) start-list '() )
  )

(define (randomize-list start-list)
  (randomize-list-n start-list (length start-list))
  )

(define (random-list n)
  (randomize-list (list-of-n n))
  )

(define (random-vals-in-range n values-needed)
  (define (build-list li len)
    (if (> len 0)
        (build-list (cons (random n) li) (sub1 len))
        li
        )
    )
  (build-list '() values-needed)
  )

; Splits a list into two - returns a two-item list, with the first and second part as members.
(define (split-list olist split-size)
  (define (split-list-iter l1 l2 n)
    (if (or (null? l2) (= n 0))
        (list (reverse l1) l2)
        (split-list-iter (cons (car l2) l1) (cdr l2) (- n 1))
        )
    )
  (split-list-iter '() olist split-size)
  )

; Verifying randomize and split-list
;(random-list 100)
;(split-list '(1 2 3 4 5 6 7 8) 3)


; The comparison tests will be run as a series, with n iterations tabulated if required for timing.  Due to possible bias from
; garbage collection time taken, the order of the tests using the same data will be randomized. 

(define (run-time-test op arglist)
  (let-values ([(r cpu real gc) (time-apply op arglist)])
    cpu
    )
  )

(define (comparison-tests n gen-test-value s1-op s2-op gen-arg1 gen-arg2 )
  (define (test-iter i time1 time2)
    (let ((s1-first (= 0 (random 2)))
          (test-val (gen-test-value) )
          )
      (let ((args1 (gen-arg1 test-val))
            (args2 (gen-arg2 test-val))
            )
        (if (= 0 i)
            (list time1 time2)  ; return the results
            (if s1-first 
                (let ((t1-results (run-time-test s1-op args1))
                      )
                  (let ((t2-results (run-time-test s2-op args2))
                        )
                    (test-iter (sub1 i)
                               (+ time1 t1-results)
                               (+ time2 t2-results)
                               )
                    )
                  )
                ; s2 is first
                (let ((t2-results (run-time-test s2-op args2))
                      )
                  (let ((t1-results (run-time-test s1-op args1))
                        )
                    (test-iter (sub1 i)
                               (+ time1 t1-results)
                               (+ time2 t2-results)
                               )
                    )
                  )
                )
            )
        )
      )
    )
  (test-iter n 0.0 0.0)
  )

; Displays the results (not perfectly formatted, but readable).
(define (timed-results n-values iterations test-fn label-1 label-2)
  (define (timed-test n)
    (let ((results (test-fn iterations n))
          )
      (display n)
      (display "        ")
      (display (car results))
      (display "        ")
      (display (cadr results))
      (newline)
      )
    )
  (display "n       ")
  (display label-1)
  (display label-2)
  (newline)
  (for-each timed-test n-values)
  )

; Test alternate versions of tree->list

(define (compare-tree-list-test n list-size)
  (comparison-tests n
                    (lambda () (list->tree (random-vals-in-range (* 2 list-size) list-size)))
                    tree->list-1
                    tree->list-2
                    (lambda (tv) (list tv))
                    (lambda (tv) (list tv))
                    )
  )

(displayln "Comparison of tree->list versions")

;(timed-results '(1 10 100 1000 10000 100000) 100 compare-tree-list-test "version 1 " "version 2")

; Unordered vs Ordered

(define (add-items-to-set set items add-method)
  (if (null? items)
      set
      (add-items-to-set (add-method (car items) set) (cdr items) add-method)
      )
  )

(define (compare-build-test n set-size)
  (comparison-tests n 
                    (lambda () (split-list (random-list set-size) (quotient set-size 2)))  ; test-value
                    (lambda (uoset new-items) (add-items-to-set uoset new-items adjoin-uoset))  
                    (lambda (oset new-items) (add-items-to-set oset new-items adjoin-oset))
                    (lambda(tv) (list (build-uoset (cadr tv)) (car tv)))
                    (lambda(tv) (list (build-oset (cadr tv)) (car tv)))
                    )
  )

; Test where the added element is always already in the set
(define (compare-adjoin-test n set-size)
  (let ((uoset (build-uoset (random-list set-size)))
        (oset  (build-oset (reverse (list-of-n set-size))))
        )
    (comparison-tests n
                      (lambda () (random set-size))
                      adjoin-uoset
                      adjoin-oset
                      (lambda(tv) (list tv uoset))
                      (lambda(tv) (list tv oset))
                      )
    )
  )

(define (compare-search-test n set-size)
  (let ((test-list (random-vals-in-range (* 2 set-size) set-size))
        )
    (let ((set1 (build-set-1 test-list))
          (set2 (build-set-2 test-list))
          )
      (comparison-tests n
                        (lambda () (random (* 2 set-size))) ; test-value
                        element-of-set-1?
                        element-of-set-2?
                        (lambda (tv) (list tv set1))
                        (lambda (tv) (list tv set2))
                        )
      )
    )
  )



; Time tests
(newline)
(displayln "Ordered vs. Unordered tests") ; (See also search tests below)


(displayln "Build tests")
(displayln "_10 iterations_")
(timed-results '(1 10 100 1000) 10 compare-build-test "Unordered " "Ordered" )
;
(displayln "Adjoin tests")
(displayln "_100 iterations_")
(timed-results '(1 10 100 1000 10000) 100 compare-adjoin-test "Unordered " "Ordered")


; Alternate tree methods


; Alternate method, attempting to use the tree directly
; First save the old methods

(define (union-set-t tset1 tset2)
  (list->tree (union-oset (tree->list tset1) (tree->list tset2)))
  )

(define (intersection-set-t tset1 tset2)
  (list->tree (intersection-oset (tree->list tset1) (tree->list tset2)))
  )

(define build-set-t build-set)


; These methods are not necessarily O(n), however, and in general do not produce balanced trees.

(define (union-set-t2 tset1 tset2)
  (cond ((null? tset1) tset2)
        ((null? tset2) tset1)
        (else
         (adjoin-set (entry tset1) (union-set-t2 (right-branch tset1)
                                                 (union-set-t2 (left-branch tset1) tset2)
                                                 )
                     )
         )
        )
  )

(define (intersection-set-t2 tset1 tset2)
  (if (or (null? tset1) (null? tset2))
      '()
      (let ((m1 (entry tset1))
            (m2 (entry tset2))
            )
        (cond
          ((= m1 m2)
           (make-tree m1
                      (intersection-set-t2 (left-branch tset1) (left-branch tset2))
                      (intersection-set-t2 (right-branch tset1) (right-branch tset2))
                      )
           )
          ((< m1 m2)
           (union-set-t2 (intersection-set-t2 (make-tree m1 (left-branch tset1) '()) (left-branch tset2))
                         (intersection-set-t2 (right-branch tset1) tset2)
                         )
           )
          ((> m1 m2)
           (union-set-t2 (intersection-set-t2 (make-tree m1 '() (right-branch tset1)) (right-branch tset2))
                         (intersection-set-t2 (left-branch tset1) tset2)
                         )
           )
          )
        )
      )
  )
;
; These and following tests assume set building, etc. are all tree-based

;(displayln "Observing alternate tree set operations")
;(union-set tree-1 tree-2)  ; { 1 3 5 7 9 11}
;(intersection-set tree-1 tree-2) ; { 1 3 5 7 9 11}
;(union-set tree-5 '())    ; { 8 7 12}
;(union-set tree-5 tree-1) ; { 1 3 5 7 8 9 11 12 }
;(intersection-set tree-5 tree-1) ; { 7 }
;(intersection-set tree-4 tree-1) ; { }
;(union-set tree-4 tree-3) ; { 1 2 3 4 5 7 9 11 12 15 18 20 24 }
;
;
;(displayln "Testing alternate method using trees directly")
;(set-op-tests)
;

(newline)
(displayln "Comparing set operations using trees")

(define (tree-union-test n set-size)
  (comparison-tests n
                    (lambda() (list (build-set (list-of-n set-size))        
                                    (build-set (list-of-n set-size))
                                    )
                      )  ; test value
                    union-set-t
                    union-set-t2
                    (lambda(tv) tv)
                    (lambda(tv) tv)
                    )
  )

(define (tree-intersection-test n set-size)
  (let ((set-limit (quotient set-size 3))
        (tlist (list-of-n set-size))  
        )
    (comparison-tests n
                      (lambda()  (list 
                                  (build-set (car (split-list tlist (random (* 2 set-limit)))))
                                  (build-set (cadr (split-list tlist (+ set-limit (random (* 2 set-limit)))) ))
                                  )
                        )  ; test value
                      intersection-set-t
                      intersection-set-t2
                      (lambda(tv) tv)
                      (lambda(tv) tv)
                      )
    )
  )

(timed-results '(1 10 100 1000 3000) 100 tree-union-test "Conversion " "Tree-only")
(timed-results '(3 30 300 3000 30000) 10 tree-intersection-test "Conversion " "Tree-only")

(newline)
(displayln "Set element search tests")

(define build-set-1 build-uoset)
(define build-set-2 build-oset)
(define element-of-set-1? element-of-uoset?)
(define element-of-set-2? element-of-oset?)

(timed-results '(1 10 100 1000 10000) 1000 compare-search-test "Unordered " "Ordered")

(define build-set-1 build-oset)
(define build-set-2 build-set)
(define element-of-set-1? element-of-oset?)
(define element-of-set-2? element-of-set?)

(timed-results '(1 10 100 1000 10000 30000) 1000 compare-search-test "Ordered   " "Tree")


; Section 3.3.3.

(define true #t)
(define false (not true))

; Be sure to select the correct line for loading exception code in test-functions.scm
(define lib-path "library/")
(define (load-from-lib file)
  (load (string-append lib-path file))
  )
  
(load-from-lib "test-functions.scm")

;; Convenience definitions
(define (displayln m)
  (display m)
  (newline)
  )

; Ex. 3.24.
; Make-table with supplied equality operation

(define (assoc-with-test key records equality-test)
  (cond ((null? records) #f)
        ((equality-test key (caar records)) (car records))
        (else (assoc-with-test key (cdr records) equality-test))
        )
  )

(define (make-table same-key?) 
  (let ((local-table (list '*table*)))
    (define (lookup key) 
      (let ((record (assoc-with-test key (cdr local-table) same-key?)))
        (if record 
            (cdr record)
            #f
            )
        )
      )
    
    (define (insert! key value)
      (let ((record (assoc-with-test key (cdr local-table) same-key?)))
        (if record 
            (set-cdr! record value) 
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)
                            )
                      )
            )
        )
      'ok
      )
    
    ; Extra definition
    (define (show-record record)
      (display (car record))
      (display ": ")
      (display (cdr record))
      (newline)
      )
    
    (define (dispatch m)                       
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'show) (lambda () (map show-record (cdr local-table))) ) 
            (else (error "Unknown operation -- TABLE" m))
            )
      )
    dispatch
    )
  )


; Testing

(define (get table key)
  ((table 'lookup) key)
  )

(define (put table key value)
  ((table 'insert!) key value)
  )

(define (show-table table)
  ((table 'show))
  (display "")  ; Could also indicate end of table
  )

; Test Sequence
(define (table-test table)
  (put table (* 9.0 5.0) 'nine-times-five)
  (put table 46 'forty-six)
  (test-equal (lambda () (get table 45.0)) 'nine-times-five "Exactly equal values match on retrieval.")
  (put table (/ 270.000007 6) 'forty-five )  ; Replaces original, but only if approximation is used.
  (test-equal (lambda () (get table 45.0)) 'forty-five "Approximately equal values are stored in the same record.")
  (test-equal (lambda () (get table 45.00001)) 'forty-five "Approximately equal values can be used for retrieval")
  (test-false  (lambda () (equal? (get table 45.001) 'forty-five)) "Values out of tolerance are not retrieved as equal.")
  (put table 45.0 'five-and-forty)
  (test-equal (lambda () (get table 45.0)) 'five-and-forty "Exactly equal values are stored in the same record.")
  )

(displayln "Testing make-table with equality test (equal?) [Failures expected]")

(define t1 (make-table equal?))
(table-test t1) ; Expected to fail the two 'approximate' tests
(newline)
(show-table t1)

(newline)
(displayln "Testing make-table with equality test (in-tolerance?)")

(define tolerance 0.0001)
(define (in-tolerance? a b) (< (abs (- a b)) tolerance))

(define t2 (make-table in-tolerance?))
(table-test t2)

(show-table t2)

(newline)

; Ex 3.25.
; Tables with an arbitrary number of keys

(define (table? item)
  (procedure? item)
  )

; assoc using a list of keys in arbitrary order
(define (assoc-all key-list records)
  (cond
    ((null? records) #f)
    ((all-match? key-list (caar records)) (car records))
    (else (assoc-all key-list (cdr records)))
    )
  )

; returns a list with any element equal to el removed (the element need not be in the list)

(define (remove el li)
  (filter (lambda (x) (not (equal? x el))) li)
  )

(define (filter predicate sequence)
  (cond ((null? sequence) '() )
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))
               )
         )
        (else (filter predicate (cdr sequence))
              )
        )
  )

; checks if the items in the lists match (arbitrarily ordered, duplicates in l2 ignored)  
(define (all-match? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)  ; both exhausted
    ((or (null? l1) (null? l2)) #f)   ; only one is empty
    ((memq (car l1) l2) (all-match? (cdr l1) (remove (car l1) l2)))
    (else #f)
    )
  )

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list) 
      (let ((record (assoc-all key-list (cdr local-table))))
        (if record
            (cdr record)
            #f
            )
        )
      )
    
    (define (insert! key-list value)
      (let ((record (assoc-all key-list (cdr local-table))))
        (if record 
            (set-cdr! record value) 
            (set-cdr! local-table
                      (cons (cons key-list value)
                            (cdr local-table)
                            )
                      )
            )
        )
      'ok
      )
    
    ; Extra definition
    (define (show-record record)
      (display (car record))
      (display ": ")
      (display (cdr record))
      (newline)
      )
    
    (define (show-table-list tlist)
      (if (not (null? tlist))
          (begin
            (show-record (car tlist))
            (show-table-list (cdr tlist))
            )
          )
      )
    
    (define (show-local-table)
      (displayln "Table:")
      (show-table-list (cdr local-table))
      (displayln "----")
      )
          
    (define (dispatch m)                       
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!) 
            ((eq? m 'show) show-local-table)
            (else (error "Unknown operation -- TABLE" m))
            )
      )
    dispatch
    )
  )

; lookup and insert! must use this signature
(define (lookup key-list table)
  (get table key-list)
  )

(define (insert! key-list value table)
  (put table key-list value)
  )

; Testing

(define (insert-and-test table keys value)
  (test-equal (lambda () (insert! keys value table)
                         (lookup keys table)
                         )
               value 
               "Insertion test"
              )
  )

(displayln "Testing tables with an arbitrary number of keys")

(define t3 (make-table))

(insert-and-test t3 '(continent-total) 7 )
;(insert-and-test t3 '(continents asia) 48)
(test-false (lambda () (lookup '(planets) t3)) "Invalid keys return false")
(insert-and-test t3 '(continents south-america) 19)
(insert-and-test t3 '(continents europe germany thuringia gotha) 11)
(insert-and-test t3 '(continents europe germany kreise) 439)
(test-false (lambda () (lookup '(continents antarctica) t3)) "Partially valid key list returns false")
(test-false (lambda () (lookup '(planets mars) t3)) "Invalid key list returns false")
; Change/update an existing value
(insert-and-test t3 '(continents asia japan) 47) ; modified from original test
(insert-and-test t3 '(continents asia) 52)

((t3 'show)) ; display table

(displayln "Checking behavior when key list is altered.")
; What happens if a key is added to an existing key list?
(insert-and-test t3 '(continents south-america brazil) 25)

; Additional tests added for this implementation
(test-equal (lambda () (lookup '(continents south-america) t3)) 19 "Records can be stored in partially matching key sets")
(insert-and-test t3 '(continents asia) 48)
(test-equal (lambda () (lookup '(continents asia japan) t3)) 47 "Records in larger key sets are preserved on insertion")

; Can keys be stored/retrieved in arbitrary order?
(displayln "Checking if key lists are arbitrarily ordered")
(lookup '(germany europe continents kreise) t3)
(insert! '(schleswig-holstein germany europe continents) 12 t3)
(insert! '(asia continents) 50 t3)
(lookup '(continents asia) t3)

(newline)

; Ex. 3.26

; Use tree implementation from 2.3.3 :

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
(define (make-tree entry left right)
  (list entry left right)
  )

; Use key-value storage for each entry in the tree 
(define (key entry) (car entry))
(define (value entry) (cdr entry))

; Retrieval can be done using modified procedure from Ex. 2.66:
(define (lookup-tree given-key tree ordering)
  (if (null? tree) false
      (let ((x given-key)
            (record (value (entry tree)))
            (record-key (key (entry tree)))
            )
        (cond 
          ((= 0 (ordering x record-key))
           record
           )
          ((< 0 (ordering x record-key))
           (lookup-tree x (left-branch tree) ordering)
           )
          ((> 0 (ordering x record-key))
           (lookup-tree x (right-branch tree) ordering)
           )
          )
        )
      )
  )

; Insertion done using modified adjoin-set from 2.65

(define (adjoin-kv-set x set ordering)
    (cond ((null? set) (make-tree x '() '()))
          ((= 0 (ordering (key x) (key (entry set))))
           (make-tree x
                      (left-branch set)
                      (right-branch set)
                      )
           )
          ((< 0 (ordering (key x) (key (entry set))))
           (make-tree (entry set)
                      (adjoin-kv-set x (left-branch set) ordering)
                      (right-branch set)
                      )
           )
          ((> 0 (ordering (key x) (key (entry set))))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-kv-set x (right-branch set) ordering)
                      )
           )
          )
  )

; Table interactions (using original format for procs)

(define (stored-ordering-proc table)
  (cdar table)
  )

(define (lookup key table)
  (lookup-tree key (cdr table) (stored-ordering-proc table))
  )

(define (insert! key value table)
  (set-cdr! table
            (adjoin-kv-set (cons key value) (cdr table) (stored-ordering-proc table))
            )
  'ok
  )

; Using a list, so the tree (cdr) is initially an empty list
(define (make-table ordering-proc)
  (list (cons '*table* ordering-proc))
  )

(define asa-table (make-table -))

(lookup 3 asa-table) ; should be false
(insert! 3 'kusi-oboadum asa-table)
(insert! 12 'kwaku-dua-II asa-table)
(lookup 12 asa-table)
(insert! 7 'osei-bonsu asa-table)
(insert! 6 'opoku-fofei asa-table)
(lookup 7 asa-table) 
(lookup 5 asa-table) ; should be false

; Ex. 3.27

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))
                 )
              )
        )
  )

; cpu time taken:
; 10: 0
; 20: 4
; 30: 626
; 40: 62166
(time (fib 30))

; Going back to original table definitions from text

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))
        )
  )

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)
    )
  )

(define (insert! key value table)
  (let ((record (assoc key (cdr table)))
        )
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))
                  )
        )
    )
  'ok
  )

(define (make-table)
  (list '*table*)
  )

(define (memoize f)
  (let ((table (make-table))
        )
    (lambda (x)
      (let ((previously-computed-result (lookup x table))
            )
        (or previously-computed-result
            (let ((result (f x))
                  )
              (insert! x result table)
              result
              )
            )
        )
      )
    )
  )

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))
                            )
                         )
                   )
             )
           )
  )


(time (memo-fib 30))
(time (memo-fib 31))

(define memo-fib2 (memoize fib))

(time (memo-fib2 30))
(time (memo-fib2 30))
(time (memo-fib2 31))

; cpu time taken:
; 20: 0
; 30: 1
; 40: 1
; 50: 1
; 1000: 72 


; Inductively - the number of steps to compute f(n) = steps for f(n-2) + some constant (whether L-t-R or R-t-L)

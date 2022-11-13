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

; >> define make-table <<


; Testing

; Modify as necessary for the table's lookup and insert procedures
(define (get table key)
  ;((table 'lookup) key)
  )

(define (put table key value)
  ;((table 'insert!) key value)
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

(displayln "Testing make-table with equality test (in-tolerance?)")

(define tolerance 0.0001)
(define (in-tolerance? a b) (< (abs (- a b)) tolerance))

(define t2 (make-table in-tolerance?))
(table-test t2)
(newline)

; Ex 3.25.
; Tables with an arbitrary number of keys

; >> Define multi-key tables << 

; lookup and insert! must use this signature
(define (lookup key-list table)
  ; >>retrieve value from table using key-list<<
  )

(define (insert! key-list value table)
  ; >>store the value in the table<<
  )

; Testing/Verification

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
(insert-and-test t3 '(continents asia) 48)
(test-false (lambda () (lookup '(planets) t3)) "Invalid keys return false")
(insert-and-test t3 '(continents south-america) 19)
(insert-and-test t3 '(continents europe germany thuringia gotha) 11)
(insert-and-test t3 '(continents europe germany kreise) 439)
(test-false (lambda () (lookup '(continents antarctica) t3)) "Partially valid key list returns false")
(test-false (lambda () (lookup '(planets mars) t3)) "Invalid key list returns false")
; Change/update an existing value
(insert-and-test t3 '(continents asia) 52)

(displayln "Checking behavior when key list is altered.")
; What happens if a key is added to an existing key list?
(insert-and-test t3 '(continents asia japan) 47)
(lookup '(continents asia) t3)
(newline)

; Ex. 3.26

; No actual implementation required, just a description

; Example of some ordered table data, to experiment
; (define asa-table << make an ordered table >>

;(lookup 3 asa-table) ; should be false
;(insert! 3 'kusi-oboadum asa-table)
;(insert! 12 'kwaku-dua-II asa-table)
;(lookup 12 asa-table)
;(insert! 7 'osei-bonsu asa-table)
;(insert! 6 'opoku-fofei asa-table)
;(lookup 7 asa-table) 
;(lookup 5 asa-table) ; should be false

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

;(time (fib 30)) ; optional, for comparison

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


;(time (memo-fib 30)) ; optional, for comparison

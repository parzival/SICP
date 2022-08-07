; Polynomial tests for 2.5.3

(load-from-lib "test-functions.scm")

(define show-operations false)  ; Can be set true to display testing operations

; poly-0 should be equal to 0, poly-1 should be non-zero, and poly-1 + neg-poly-1 should be zero.
(define (poly-zero-test poly-0 poly-1 neg-poly-1)
  (test-true (lambda () (=zero? poly-0)) "poly-0 is =zero?")
  (test-false (lambda () (=zero? poly-1)) "poly-1 is not =zero?")
  (test-true (lambda () (=zero? (add poly-1 neg-poly-1))) "poly-1 + neg-poly-1 is =zero?")
  (test-true (lambda () (=zero? (mul poly-1 poly-0))) "poly-1 * poly-0 is =zero?")
  (displayln "=zero? tests passed.")
  )

; Polynomial binary op test

; The input list requires 7 polynomials. 
; It uses the labels poly-0 through poly-4 for the first 5.  
; poly-0 must =zero?
;
; input-list : 7 polynomials. All but the last must be in the same variable.
; These are referred to in sequence as poly-0 through poly-4, poly-id and poly-y.
;  poly-0 should equal zero.
;  poly-1 through poly-4 can have any values; see the results list for how they are used.
;  poly-id should be the identity for the operation, such that any-poly op poly-id = any-poly (only valid if ident is true)
;  poly-y, the last polynomial should be a different variable from the first 6. It does not have to be in 'y'.
;
; expected-result-list: Contains expected results for calculating, in order-
;                        poly-2 op poly-1
;                        poly-4 op poly-3
;                        poly-2 op poly-4
;
; op : the actual operation (as a procedure)
; op-sym : the symbol used to display the operation (e.g. '+' for add)
; ident:, assoc:, commute: Boolean values indicating whether the tests related to those properties should be performed.

;
(define (poly-binop-test input-list expected-result-list op op-sym ident assoc commute)
  (define (show-op p1 p2)  ; note that showing operations will result in a program exit if there is an error
    (if show-operations
        (begin
        (display p1)
        (display " ")
        (display op-sym)
        (display " ")
        (displayln p2)
        (display "= ")
        (display (op p1 p2))
        (newline)
        )
        )
    )
  (let ((poly-0 (list-ref input-list 0))
        (poly-1 (list-ref input-list 1))
        (poly-2 (list-ref input-list 2))
        (poly-3 (list-ref input-list 3))
        (poly-4 (list-ref input-list 4))
        (poly-id (list-ref input-list 5)) 
        (poly-y (list-ref input-list 6))  ; poly does not have to be in y, just a different variable
        )
    (display "Testing polynomial operation:")
    (display op-sym)
    (newline)
    
    ; identity test
    (if ident (begin
                (show-op poly-2 poly-id)
                (test-equ (lambda () (op poly-2 poly-id)) poly-2 "identity")
                )
        )
    ; associative test
    (if assoc (begin
                (if show-operations 
                    (printf "(poly-1 ~a poly-2) ~a poly-3 = poly-1 ~a (poly-2 ~a poly-3)" op-sym op-sym op-sym op-sym)
                    )
                (test-equ (lambda () (op (op poly-1 poly-2) poly-3)) (op poly-1 (op poly-2 poly-3)) "associativity")
                )
        )
    ; commutativity test
    (if commute (begin
                  (if show-operations (printf "poly-3 ~a poly-4 = poly-4 ~a poly-3" op-sym op-sym))
                  (test-equ (lambda () (op poly-3 poly-4)) (op poly-4 poly-3) "commutativity")
                  )
        )
    ; operations tests
    (show-op poly-2 poly-1)
    (test-equ (lambda () (op poly-2 poly-1)) (list-ref expected-result-list 0) "op: poly-2, poly-1")
    (show-op poly-4 poly-3)
    (test-equ (lambda () (op poly-4 poly-3)) (list-ref expected-result-list 1) "op: poly-4, poly-3")
    (show-op poly-2 poly-4)   
    (test-equ (lambda () (op poly-2 poly-4)) (list-ref expected-result-list 2) "op: poly-2, poly-4")
    
    (test-for-failure (lambda () (op poly-2 poly-y))
      (format "Polynomials in different variables: ~a ~a ~a ." poly-2 op-sym poly-y)
      )
    (newline)
    )
  )

; Each of these tests uses poly-binop test to run a series of basic tests.  See the definition of poly-binop for the requirements on the input and expected result lists.

(define (poly-add-test input-list expected-result-list) 
  (poly-binop-test input-list expected-result-list add '+ true true true)
  (displayln "Addition tests completed.")
  )

(define (poly-sub-test input-list expected-result-list)
  (poly-binop-test input-list expected-result-list sub '- true false false)
  (displayln "Subtraction tests completed.")
  ) 
  
(define (poly-mul-test input-list expected-result-list)
  (poly-binop-test input-list expected-result-list mul '* true true true)
  (let ((poly-0 (list-ref input-list 0))
        ;(poly-1 (list-ref input-list 1))
        (poly-2 (list-ref input-list 2))
        )
    ; Additional tests beyond basic binop test
    ; multiplication by zero test
    (displayln "Additional multiplication tests")
    (test-true (lambda () (=zero? (mul poly-0 poly-2))))
    (newline)
    (displayln "Multiplication tests completed.")
    )
  )

; For the division tests, the expected results should be in the order of quotient followed by remainder
(define (poly-div-test input-list expected-result-list)
  (define (show-div p1 p2)
    (if show-operations
        (show-binop p1 p2 "/" div)
        )
    )
  (let ((poly-2 (list-ref input-list 2))
        (poly-3 (list-ref input-list 3))
        (poly-id (list-ref input-list 5))
        (quotient-list (list (list-ref expected-result-list 0)
                                  (list-ref expected-result-list 2)
                                  (list-ref expected-result-list 4)
                                  )
                            )
        (remainder-list (list (list-ref expected-result-list 1)
                                  (list-ref expected-result-list 3)
                                  (list-ref expected-result-list 5)
                                  )
                            )
        )
    (poly-binop-test input-list quotient-list (lambda(x y) (poly-quotient (div x y))) "/" true false false) 
    (poly-binop-test input-list remainder-list (lambda(x y) (poly-remainder (div x y))) "/rem" false false false)
    (displayln "Additional division tests")
     ; identity test
    (test-true (lambda () (=zero? (poly-remainder (div poly-2 poly-id)))) "identity remainder" )
    ; division by self
    (test-true (lambda () (=zero? (poly-remainder (div poly-3 poly-3)))) "self-division remainder")   
    )
  
  (displayln "Division tests completed.")
  )

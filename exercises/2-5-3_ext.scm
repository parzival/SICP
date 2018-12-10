; Section 2.5.3 (extended exercise)

; Required generic arithmetic package from previous exercises,
; with polynomial division defined

(load "library/gen-arith.scm")           ; Give location of your previously defined arithmetic file.
(load "library/gen-arith-tests_v2.scm")  ; Note that only tower types are defined; scheme-number tests should not be run.
(load "library/poly-tests.scm")          ; Polynomial tests


;(run-tower-arith-tests) ; optional check to ensure all files are loaded correctly

(define (header msg)
  (newline)
  (display msg)
  (newline)
  )

(load "2-5-3_sol.scm") ; load previous exercises, with their polynomial system

(header "========================== START OF EXTENDED EXERCISE ==========================")
;(require trace)  - can be useful for debugging/answering 2.95

; Ex 2.93
; Creating rational functions

; Modify the rational package to allow the use of polynomials as
; elements.  Change it to not reduce fractions using gcd.



; Testing 
(header "Testing rational number package with non-polynomial values...")
(rational-arith-tests)  ; run previous tests for rationals, to ensure it still functions

; Example from text
(displayln "Demonstrating rational polynomial")
(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1)))) 
(define rf (make-rational p2 p1))
(displayln "Adding p2/p1 + p2/p1")
(add rf rf)

; Ex 2.94.
; GCD for rational polynomials

; Euclid's algorithm
;  (define (gcd-terms a b) 
;    (if (empty-termlist? b)
;        a
;        (gcd-terms b (remainder-terms a b))
;        )
;    )

; Write gcd-poly to calculate the gcd for polynomials

(define (greatest-common-divisor a b)
  ; use gcd-poly for polynomials, regular gcd for other values
  )

; Testing

;(run-tower-arith-tests) ; optional tests to ensure no errors due to changes made

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))

; Find the correct coefficients by hand
;(define expected-result (make-polynomial ... 

(header "Testing greatest-common-divisor with various types")
(test-equ (lambda () (greatest-common-divisor (make-integer 15) (make-integer 12))) (make-integer 3) "Integers" )  
(test-equ (lambda () (greatest-common-divisor p1 p2)) expected-result "Polynomials")
(test-equ (lambda () (greatest-common-divisor (make-rational 200 5) (make-rational 64 4))) (make-integer 8) "Rationals")

; Unspecified results
(display "Scheme integers: ")
(greatest-common-divisor 90 54)  ; 18 (if scheme-numbers are allowed)
(display "Non-integer rationals: ")
(greatest-common-divisor (make-rational 221 11) (make-rational 169 22)) ; 13/22 is a possible answer, but non-integer
(display "Reals: ")
(greatest-common-divisor (make-real 50.5) (make-real 2.25))  ; 0.25 is a possible answer, but non-integer 
; no specified value

; Ex 2.95.
; Non-integer complications

(header "Demonstrating result of non-integer operations")

; define three polynomials as follows
(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

; define q1 as product of p1 and p2, and q2 as p1 * p3.

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

; Compute the GCD of q1 and q2.  Note that it is not p1.
(displayln "GCD of q1 and q2 (failure expected):")
(test-equ (lambda () (greatest-common-divisor q1 q2)) p1)
(greatest-common-divisor q1 q2)

; Explain the result


; Ex 2.96.
; Integerizing polynomials for GCD.

; a. Implement (pseudoremainder-terms) to multiply the terms of the dividend
; by an integerizing coefficient

(header "Integerizing polynomial division")

(displayln "Testing GCD of q1 and q2 (integerized, failure expected)")

(test-equ (lambda () (greatest-common-divisor q1 q2)) p1)  ; Still a failure
; Still not p1, but now has integer coeffecients


; b. Modify GCD-terms to remove common factors in the result, so that 
; integerized values do not end up with large coefficients.


; Testing

(header "Testing GCD of q1 and q2 (removing common factors)")
(test-equ (lambda () (greatest-common-divisor q1 q2)) p1)


; Ex 2.97.
; Reducing to lowest terms


(define (reduce a b)
  ; generic operation
  )

; Testing

(header "Rational package using reduce and generic operations")

(displayln "Ensuring original tests all pass")
(run-tower-arith-tests)

; Polynomial tests

; Inputs for addition and subtraction tests
(define input-list (list (make-polynomial 'x '((0 0)))                 ; zero
                         (make-polynomial 'x (list (list 1 (make-integer 1))))    ; poly-1
                         (make-polynomial 'x (list (list 1 (make-real -4.0)) (list 0 (make-integer 7)) ))           ; poly-2
                         (make-polynomial 'x (list (list 3 (make-complex-from-real-imag 7 -2)) (list 2 (make-rational -32 12)) (list 1 (make-integer 5)) )) ; poly-3
                         (make-polynomial 'x (list (list 4 (make-integer -1)) (list 3 (make-complex-from-mag-ang 2.0 (/ pi 2.0) )) (list 2 (make-rational 2 3)) (list 1 (make-real -3.3)) (list 0 (make-complex-from-real-imag -4 -3)) ))  ; poly-4
                         (make-polynomial 'x '((0 0)))                 ; identity in x
                         (make-polynomial 'y '((2 2) (1 4) (0 8)))     ; poly in another variable
                         ) 
  )

; Inputs for multiplication test
(define mul-input-list (list (make-polynomial 'x '((0 0)) )               ; zero
                             (make-polynomial 'x (list (list 2 (make-integer -3)) (list 1 (make-integer 2))) )        ; poly-1
                             (make-polynomial 'x (list (list 2 (make-rational 3 -5)) (list 1 (make-integer 5)) ))        ; poly-2
                             (make-polynomial 'x (list (list 3 (make-real 6.4)) (list 2 (make-rational 40 3)) (list 0 (make-real (/ 2.0 3)))) ) ; poly-3
                             (make-polynomial 'x (list (list 4 (make-integer 13))  (list 2 (make-integer -5)) (list 1 (make-rational 6 20)) '(0 -4)) )  ; poly-4
                             (make-polynomial 'x '((0 1)) )               ; identity in x
                             (make-polynomial 't (list (list (make-rational 2 1) (list 1 (make-real 4.0)) '(0 8)) ))   ; poly in another variable
                             ) 
  )

; Run tests to see if polynomials can be made from package values
(poly-add-test input-list 
               (list (make-polynomial 'x '((1 -3) (0 7)) )  ; poly-2 + poly-1
                     (make-polynomial 'x (list '(4 -1) '(3 7.0) '(2 -2) '(1 1.7) (list 0 (make-complex-from-mag-ang 5 (+ pi 0.64350110879328))) ) ) ; poly-4 + poly-3
                     (make-polynomial 'x (list '(4 -1) (list 3 (make-complex-from-real-imag 0 2)) '(2 0.6666666666667) '(1 -7.3) (list 0 (make-complex-from-real-imag 3 -3)) )) ; poly-2 + poly-4
                     )
               )

(poly-mul-test mul-input-list 
               (list (make-polynomial 'x (list (list 4 (make-rational 9 5)) (list 3 (make-rational -81 5)) '(2 10)) ) ; poly-2 * poly-1
                     (make-polynomial 'x (list (list 7 (make-real 83.2)) (list 6 (make-rational 520 3)) '(5 -32) (list 4 (add (make-rational -174 3) (make-rational 48 25))) (list 3 (make-real -21.6)) (list 2 (make-rational -170 3)) (list 1 (make-rational 1 5)) (list 0 (make-real (/ -8.0 3)))) ) ; poly-4 * poly-3
                     (make-polynomial 'x (list (list 6 (/ -39 5)) '(5 65) '(4 3) (list 3 (make-real -25.18)) (list 2 (add (make-integer 3) (make-rational 9 10)))  '(1 -20))) ; poly-2 * poly-4
                     )
               )

(header "Testing reduce with rationals using integers/reals")

(reduce (make-integer 12) (make-integer 15)) ; 4, 5
(reduce (make-integer 4420) (make-integer 5460)) ; 17, 21
(reduce (make-integer 47) (make-integer 91)) ; 47, 91

(reduce (make-real 63.0) (make-integer 90)) ; 7, 10
(reduce (make-real 43.5) (make-real 13.5)) ; undefined

(displayln "Testing addition of polynomials using reduce")

; Example from text
(define p1 (make-polynomial 'x '((1 1)(0 1)))) 
(define p2 (make-polynomial 'x '((3 1)(0 -1))))
(define p3 (make-polynomial 'x '((1 1)))) 
(define p4 (make-polynomial 'x '((2 1)(0 -1))))

(define rf1 (make-rational p1 p2)) 
(define rf2 (make-rational p3 p4))

; Expected sum
(define rfsum (make-rational (make-polynomial 'x '((3 1) (2 2) (1 3) (0 1))) 
                             (make-polynomial 'x '((4 1) (3 1) (1 -1) (0 -1)))
                             )
  )

(test-equ (lambda () (add rf1 rf2)) rfsum)


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

; Need to have this so that polynomials don't cause errors
(put 'to-scheme-number '(polynomial) (lambda (x) false))

; If this is undefined for integers, the changes to raise for the rational package will result in an infinite loop 
(put 'to-scheme-number '(integer) contents)


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d)
    )
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x))
                   )
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (a b) (equ? (mul (numer a) (denom b)) (mul (numer b) (denom a))))
       )
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x)))
       )
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  (put 'to-scheme-number 'rational
       (lambda (x) (let ((num-n (to-scheme-number (numer x)))
                         (den-n (to-scheme-number (denom x)))
                         )
                     (if (and num-n den-n)
                         (/ num-n den-n))
                     false
                     )
         )
       )
  
  (put 'project 'rational ; to integer 
       ; This can only be done with values that can be coerced,
       ; as quotient only works on Scheme numbers.
       (lambda (x) (let ((num-n (to-scheme-number (numer x)))
                         (den-n (to-scheme-number (denom x)))
                         )
                     (if (and num-n den-n)
                         (make-integer (quotient num-n den-n))
                         false
                         )
                     )
         )
       )
  (put 'raise 'rational ; to real
       (lambda (x) (let ((num-n (to-scheme-number (numer x)))
                         (den-n (to-scheme-number (denom x)))
                         )
                     (if (and num-n den-n)
                         (make-real (/ num-n den-n))
                         x  
                         )
                     )
         )
       )
  
  'done
  )

(newline)
(display "Installing new rational package...")
(install-rational-package)

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

; This likely yields errors or unexpected results
;
;(displayln "Demonstrating generic add of incompatible items")
;(add rf (make-rational 3 5))
;(add (make-rational 4 6) rf)

; Ex 2.94.
; GCD for rational polynomials

; Write a gcd-poly to calculate the gcd for polynomials

(define (update-sparse-poly-package)
  ;; internal procedures
  ;; representation of poly
 (define (make-poly variable term-list)
    (cons variable term-list)
    )
  
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  ;; representation of terms and term lists
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)
        )
    )
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  
  ; Arithmetic operations (poly)
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  ; this is new, to make gcd terms easier
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L2)
           L1
           )
          ((empty-termlist? L1)
           (mul-term-by-all-terms (make-term 0 -1) L2)  ; negate L2
           )
          (else
           (add-terms L1
                      (mul-term-by-all-terms (make-term 0 -1) L2)  
                      )
           )
          )
    )
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  
  ; Division

  (define (quotient result) (car result))
  
  (define (remainder result) (cadr result))
  
  (define (quotient-terms a b) (quotient (div-terms a b)))
  
  (define (remainder-terms a b) (remainder (div-terms a b)))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist)) 
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1) 
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2)))
                    )
                (let ((rest-of-result
                       (div-terms (sub-terms L1
                                             (mul-term-by-all-terms (make-term new-o new-c) L2)
                                             )
                                  L2
                                  )
                       )
                      )
                  ; Return result as a list
                  (list (cons (make-term new-o new-c) (quotient rest-of-result))
                        (remainder rest-of-result)
                        )
                  )
                )
              )
          )
        )
    )
  
  ; GCD 
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- GCD-POLY" (list p1 p2))
        )
    )

  ; Euclid's algorithm
  (define (gcd-terms a b) 
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))
        )
    )
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  
  
  (put 'greatest-common-divisor '(sparse sparse)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))
         )
       )
  
  
  'done
  )


(define (update-integer-package)
  (define (tag p) (attach-tag 'integer p))
  
  (put 'greatest-common-divisor '(integer integer)
       (lambda (x1 x2) (tag (gcd x1 x2))) 
       )
  )

(define (update-real-package)
  (define (tag p) (attach-tag 'real p))
  
  (put 'greatest-common-divisor '(real real)
       (lambda (x1 x2) (tag (gcd x1 x2))) 
       )
  )

(define (update-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))

  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (greatest-common-divisor p1 p2)))
       )
  )


(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b)
  )

; Updating/reinstalling packages

(displayln "Updating/reinstalling packages")
(display "Updating sparse polynomial package...")
(update-sparse-poly-package)
(display "Updating polynomial package...")
(update-polynomial-package)
(display "Updating integer package...")
(update-integer-package)
(display "Updating real package...")
(update-real-package)

; Testing

;(run-tower-arith-tests) ; optional tests to ensure no errors due to changes made

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1)))) 
(define expected-result (make-polynomial 'x '((2 -1) (1 1)))) ; determine by hand

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
; The division of the first term leads to a rational coefficient.  Since rationals are not reduced, the fractional terms increase in size at every step, and this never gets divided out.  The resulting coefficients in the gcd are scaled by a rational value from the 'correct' form.


; Ex 2.96.
; Integerizing polynomials for GCD.

; a. Implement (pseudoremainder-terms) to multiply the terms of the dividend
; by an integerizing coefficient

(define (update-sparse-poly-package-pseudoremainder)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list)
    )
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  ;; representation of terms and term lists
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)
        )
    )
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  ; Term operations
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L2)
           L1
           )
          ((empty-termlist? L1)
           (mul-term-by-all-terms (make-term 0 -1) L2)  ; negate L2
           )
          (else
           (add-terms L1
                      (mul-term-by-all-terms (make-term 0 -1) L2)  
                      )
           )
          )
    )
  
  ; Division
  (define (quotient result) (car result))
  
  (define (remainder result) (cadr result))
  
  (define (quotient-terms a b) (quotient (div-terms a b)))
  
  (define (remainder-terms a b) (remainder (div-terms a b)))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist)) 
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1) 
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2)))
                    )
                (let ((rest-of-result
                       (div-terms (sub-terms L1
                                             (mul-term-by-all-terms (make-term new-o new-c) L2)
                                             )
                                  L2
                                  )
                       )
                      )
                  ; Return result as a list
                  (list (cons (make-term new-o new-c) (quotient rest-of-result))
                        (remainder rest-of-result)
                        )
                  )
                )
              )
          )
        )
    )
  
  
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- GCD-POLY" (list p1 p2))
        )
    )
  
  
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))
        )
    )
  
  (define (pseudoremainder-terms a b)
    (let ((intc (expt (to-scheme-number (coeff (first-term b)))
                      (- (+ 1
                            (order (first-term a))
                            )
                         (order (first-term b))
                         )
                      )
                )
          )
      (remainder (div-terms (mul-term-by-all-terms (make-term 0 intc) a)
                            b
                            )
                 )
      )
    )
  
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  
  
  (put 'greatest-common-divisor '(sparse sparse)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))
         )
       )
  
  'done
  )

(header "Integerizing polynomial division")
(display "Updating poly package (with pseudoremainder-terms) ...")
(update-sparse-poly-package-pseudoremainder)

(displayln "Testing GCD of q1 and q2 (integerized, failure expected)")

(test-equ (lambda () (greatest-common-divisor q1 q2)) p1)  ; Still a failure
; Still not p1, but now has integer coeffecients


; b. Modify GCD-terms to remove common factors in the result, so that 
; integerized values do not end up with large coefficients.

(define (update-sparse-poly-package-reduced-common-factors)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list)
    )
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  ;; representation of terms and term lists
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)
        )
    )
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  ; Term operations
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L2)
           L1
           )
          ((empty-termlist? L1)
           (mul-term-by-all-terms (make-term 0 -1) L2)  ; negate L2
           )
          (else
           (add-terms L1
                      (mul-term-by-all-terms (make-term 0 -1) L2)  
                      )
           )
          )
    )
  
  ; Division
  (define (quotient result) (car result))
  
  (define (remainder result) (cadr result))
  
  (define (quotient-terms a b) (quotient (div-terms a b)))
  
  (define (remainder-terms a b) (remainder (div-terms a b)))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist)) 
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1) 
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2)))
                    )
                (let ((rest-of-result
                       (div-terms (sub-terms L1
                                             (mul-term-by-all-terms (make-term new-o new-c) L2)
                                             )
                                  L2
                                  )
                       )
                      )
                  ; Return result as a list
                  (list (cons (make-term new-o new-c) (quotient rest-of-result))
                        (remainder rest-of-result)
                        )
                  )
                )
              )
          )
        )
    )
  
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((gcd-integer (gcd-terms (term-list p1) (term-list p2))))                     
          (make-poly (variable p1) 
                     (quotient-terms gcd-integer
                                     (list (make-term 0 (gcd-coeffs gcd-integer)))
                                     )
                     )
          )
        (error "Polys not in same var -- GCD-POLY" (list p1 p2))
        )
    )
  
  ; Get the gcd of the coefficients of a term list
  (define (gcd-coeffs tl)
    (cond
      ((empty-termlist? tl) 1)
      ((< 2 (length tl)) (coeff (first-term tl)))
      (else (greatest-common-divisor (coeff (first-term tl))
                                     (gcd-coeffs (rest-terms tl))
                                     )
            )
      )
    )
  
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))
        )
    )
  
  (define (pseudoremainder-terms a b)
    (let ((intc (expt (to-scheme-number (coeff (first-term b)))
                      (- (+ 1
                            (order (first-term a))
                            )
                         (order (first-term b))
                         )
                      )
                )
          )
      (remainder (div-terms (mul-term-by-all-terms (make-term 0 intc) a)
                            b
                            )
                 )
      )
    )
  
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  
  
  (put 'greatest-common-divisor '(sparse sparse)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))
         )
       )
  
  'done
  )


; Testing
(display "Installing poly package (removing common factors) ...")
(update-sparse-poly-package-reduced-common-factors)

(header "Testing GCD of q1 and q2 (removing common factors)")
(test-equ (lambda () (greatest-common-divisor q1 q2)) p1)


; Ex 2.97.
; Reducing to lowest terms

(define (update-sparse-poly-package-reduce)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list)
    )
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  ;; representation of terms and term lists
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)
        )
    )
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  ; New poly operation (reduce to lowest terms)
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((red-result (reduce-terms (term-list p1)
                                        (term-list p2)
                                        )
                          )
              )
          (list (make-poly (variable p1) (car red-result))
                (make-poly (variable p2) (cadr red-result))
                )
          )
        (error "Polys not in same var -- REDUCE-POLY" (list p1 p2))
        )
    )
  
  ; Term operations (all operate on term lists, or single terms)
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L2)
           L1
           )
          ((empty-termlist? L1)
           (mul-term-by-all-terms (make-term 0 -1) L2)  ; negate L2
           )
          (else
           (add-terms L1
                      (mul-term-by-all-terms (make-term 0 -1) L2)  
                      )
           )
          )
    )
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  
  ; Division accessors
  
  (define (quotient-terms a b) (quotient (div-terms a b)))
  
  (define (remainder-terms a b) (remainder (div-terms a b)))
  
  (define (quotient result) (car result))
  
  (define (remainder result) (cadr result))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist)) 
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1) 
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2)))
                    )
                (let ((rest-of-result
                       (div-terms (sub-terms L1
                                             (mul-term-by-all-terms (make-term new-o new-c) L2)
                                             )
                                  L2
                                  )
                       )
                      )
                  ; Return result as a list
                  (list (cons (make-term new-o new-c) (quotient rest-of-result))
                        (remainder rest-of-result)
                        )
                  )
                )
              )
          )
        )
    )
  
  (define (reduce-terms n d)
    (let ((gcd-t (gcd-terms n d))
          (O1 (max (order (first-term n))
                   (order (first-term d))
                   )
              )
          )
      (let ((intc-tl (list (make-term 0 
                                      (expt (to-scheme-number (coeff (first-term gcd-t)))
                                            (- (+ 1 O1) (order (first-term gcd-t)))
                                            )
                                      )
                           )
                     )
            )
        (let ((int-n (quotient-terms (mul-terms n intc-tl) gcd-t))
              (int-d (quotient-terms (mul-terms d intc-tl) gcd-t))
              )
          (let ((gcdc-term (list (make-term 0 (greatest-common-divisor (gcd-coeffs int-n)
                                                                       (gcd-coeffs int-d)
                                                                       )
                                            )
                                 )
                           )
                )
            (list (quotient-terms int-n gcdc-term)
                  (quotient-terms int-d gcdc-term)
                  )
            )
          )
        )
      )
    )
  
  ; Get the gcd of the coefficients of a term list
  (define (gcd-coeffs tl)
    (cond
      ((empty-termlist? tl) 1)
      ((< 2 (length tl)) (coeff (first-term tl)))
      (else (greatest-common-divisor (coeff (first-term tl))
                                     (gcd-coeffs (rest-terms tl))
                                     )
            )
      )
    )
  
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))
        )
    )
  
  (define (pseudoremainder-terms a b) 
    (let ((intc (expt (to-scheme-number (coeff (first-term b)))
                      (- (+ 1
                            (order (first-term a))
                            )
                         (order (first-term b))
                         )
                      )
                )
          )
      (remainder (div-terms (mul-term-by-all-terms (make-term 0 intc) a)
                            b
                            )
                 )
      )
    )
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  
  (put 'reduce '(sparse sparse)
       (lambda (p1 p2)
         (let ((red-res (reduce-poly p1 p2)))
           (attach-tag 'reduced
                 (list (tag (car red-res))
                       (tag (cadr red-res))
                       )
                 )
           )
         )
       )
  
  'done
  )

(define (update-polynomial-package-reduce)
  (define (tag p)
    (attach-tag 'polynomial p)
    )

  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         ; Get the reduced lower polys, strip the 'reduced tag, add poly tags, then add our reduced tag
           (attach-tag 'reduced (map (lambda (el) (tag el)) (cdr (reduce p1 p2))))
         )
       ) 

  )

; Reduce for integers, so that the system works
(define (update-integer-package-reduce)
  
  (define (tag x)
    (attach-tag 'integer x)
    )
  
  (define (reduce-integers n d) 
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))
      )
    )
  
  (put 'reduce '(integer integer)
       (lambda (n d)
         (let ((red-res (reduce-integers n d)))
           (attach-tag 'reduced
                 (list (tag (car red-res))
                       (tag (cadr red-res))
                       )
                 
                 )
           )
         )
       )
  )

(define (update-complex-package-reduce)
  
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  (define (tag z) (attach-tag 'complex z))
  
  (define (conjugate c)
    (make-complex-from-real-imag (real-part c) (sub (make-real 0.0) (imag-part c)))
    )
  
  (define (reduce-rationals n d) 
    (let ((g (greatest-common-divisor n d)))
      (make-rational (div n g) (div d g))
      )
    )
  
  (define (reduce-complex cn cd)
    (let ((real-denom (add (mul (real-part cd) (real-part cd)) (mul (imag-part cd) (imag-part cd)))) ; this is exact to force real values
          (new-num (mul (tag cn) (conjugate cd)))
          )
      (make-from-real-imag (reduce-rationals (real-part new-num) real-denom)
                           (reduce-rationals (imag-part new-num) real-denom)
                           )
      )
    )
  
  (put 'reduce '(complex complex)
       (lambda (n d)
         (tag (reduce-complex n d))
         )
       )
  )

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (let ((red-num (car (contents reduced)))
            (red-den (cadr (contents reduced)))
            )
        (cons red-num red-den)
        )
      )
    )
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x))
                   )
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'equ? '(rational rational)
       (lambda (a b) (equ? (mul (numer a) (denom b)) (mul (numer b) (denom a))))
       )
  
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x)))
       )
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  (put 'project 'rational ; to integer 
       (lambda (x) (let ((num-n (to-scheme-number (numer x)))
                         (den-n (to-scheme-number (denom x)))
                         )
                     (if (and num-n den-n)
                         (make-integer (quotient num-n den-n))
                         false
                         )
                     )
         )
       )
  
  (put 'raise 'rational ; to real
       (lambda (x) (let ((num-n (to-scheme-number (numer x)))
                         (den-n (to-scheme-number (denom x)))
                         )
                     (if (and num-n den-n)
                         (make-real (/ num-n den-n))
                         (error "Cannot raise rational to real : " x)
                         )
                     )
         )
       )
  'done
  )

(define (reduce a b)
  (apply-generic 'reduce a b)
  )

(put 'drop 'reduced
     (lambda (x)
       (drop x)
       )
     )

; Testing

(header "Rational package using reduce and generic operations")

; Updating/reinstalling packages

(displayln "Updating/reinstalling packages")
(display "Updating polynomial packages...")
(update-sparse-poly-package-reduce)
(update-polynomial-package-reduce)
(display "Updating integer package...")
(update-integer-package-reduce)
(display "Updating complex package...")
(update-complex-package-reduce)
(display "Installing rational package...")
(install-rational-package)

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

; Some uncovered aspects of the system 
(displayln "Examining operations with other values and polynomials")
(display "5 + p1 = ")
(add (make-integer 5) p1)
(display "7.4 + p1 = ")
(add p1 (make-real 7.4))
(display "3 * p2 =")
(mul (make-integer 3) p2)
(display "p2 * 0.25 = ")
(mul p2 (make-real 0.25))

;(add rf1 (make-integer 11)) ; Results in raising error
(add (make-integer 9) rf1) ; Correct, but not properly reduced results
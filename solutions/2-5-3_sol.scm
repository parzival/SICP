; Section 2.5.3

; Required table operations and arithmetic package
;(require racket/mpair)  ; May be required for the debugger

(load "library/gen-arith.scm")           ; Give location of your previously defined arithmetic file.
(load "library/gen-arith-tests_v2.scm")  ; Note that only tower types are defined; scheme-number tests should not be run.
(load "library/poly-tests.scm")          ; Polynomial tests

; Display a testing/informational message
(define (header msg)
  (newline)
  (displayln msg)
  )

;(header "Testing Base Arithmetic System")
;(run-tower-arith-tests) ; optional pre-test to ensure system is functioning

; Original polynomial package (from text)
(define (install-polynomial-package)
  ;; internal procedures
  
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
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
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2)))
    )
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2)))
    )
  
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
  
  ; Required for testing
  (define (mul-poly-by-c p c)
    (mul-poly p (make-poly (variable p) (list (list 0 (attach-tag 'complex c))) ))  ; Using knowledge of complex numbers
    )
  
  (put 'mul '(polynomial complex)
       (lambda (p c) (tag (mul-poly-by-c p c))) 
       )
  
  
  (put 'mul '(complex polynomial)
       (lambda (c p) (tag (mul-poly-by-c p c)))
       )  
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done
  )

;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(newline)
(display "Installing polynomial package ...")
(install-polynomial-package)

; Ex 2.87.
; Implementing =zero? for polynomials

; Install a =zero? function for polynomials, so that
; polynomials can have polynomial coefficients.

(define (update-polynomial-package)
  
  ; internal procedures (unchanged from previous)
  ; All internal procedures for poly
  (define (make-poly variable term-list)
    (cons variable term-list))
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
  
  (define (tag p) (attach-tag 'polynomial p))
  
  ; Required for testing
  (define (mul-poly-by-c p c)
    (let ((retagged (attach-tag 'complex c))
          ); Mul op gives us bare contents of the complex number, so we need to use attach-tag 
      (mul-poly p (make-poly (variable p) 
                             (list (list 0 retagged))  
                             )
                )
      )
    )
  
  (put 'mul '(polynomial complex)
       (lambda (p c) (tag (mul-poly-by-c p c))) 
       )
  
  
  (put 'mul '(complex polynomial)
       (lambda (c p) (tag (mul-poly-by-c p c)))
       )  
  
  ; end of previous internal-procedures
  
  
  
  (define (equal-zero? term-list)
    (if (empty-termlist? term-list)
        true
        (and (=zero? (coeff (first-term term-list))) (equal-zero? (rest-terms term-list)))
        )
    )
  
  (put '=zero? '(polynomial) 
       (lambda (p) (equal-zero? (term-list p)))
       )
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  )

(update-polynomial-package)

; Testing ...

(header "Testing polynomials: =zero?")

; In poly-zero-test, the first polynomial should be =zero
; The second polynomial should be non-zero.
; The third should be the 'negative' of the second (sum to zero).  
; Add and mul operations must be defined for polynomials 

; Basic tests for zero
(poly-zero-test (make-polynomial 'x '((0 0)))     ; =zero
                (make-polynomial 'x '((1 1)))     ; non-zero
                (make-polynomial 'x '((1 -1)))    ; sums to zero with previous
                )

; Tests using a different value for order/coefficient
(poly-zero-test (make-polynomial 'y '())           ; =zero
                (make-polynomial 'y (list (list 2 (make-real  5.3)))) ; non-zero
                (make-polynomial 'y (list (list 2 (make-real -5.3)))) ; sums to zero with previous
                )

; Tests using multiple terms
(poly-zero-test (make-polynomial 'z '((1 0) (0 0)))  ; =zero
                (make-polynomial 'z '((2 2) (1 -1) (0 0))) ; non-zero
                (make-polynomial 'z '((2 -2) (1 1)))       ; sums to zero with previous
                )


; Tests using polynomials as coefficient
; (as mentioned in the text)

(poly-zero-test (make-polynomial 'a '((0 0)))     ; =zero
                (make-polynomial 'a (list (list 2 (make-polynomial 'b '((2 1) (0 1)) )) ; non-zero
                                          (list 1 (make-polynomial 'b '((1 2)) ))
                                          (list 0 (make-polynomial 'b '((2 1)) ))
                                          )
                                 )
                (make-polynomial 'a (list (list 2 (make-polynomial 'b '((2 -1) (0 -1)) )) ; sums to zero with previous
                                          (list 1 (make-polynomial 'b '((1 -2)) ))
                                          (list 0 (make-polynomial 'b '((2 -1)) ))
                                          )
                                 )
                )

; Ex 2.88.
; Polynomial subtraction

; Extend the system to include polynomial subtraction


(define (update-polynomial-package)
  ; Original internal procedures for poly
  (define (make-poly variable term-list)
    (cons variable term-list))
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
  
  (define (tag p) (attach-tag 'polynomial p))
  ; end of original internal procedures
  
  ; Used for testing if term-list is equal to zero
  (define (equal-zero? term-list)
    (if (empty-termlist? term-list)
        true
        (and (=zero? (coeff (first-term term-list))) (equal-zero? (rest-terms term-list)))
        )
    )
  
  
  ; Polynomials are equal if they are in the same variable, and all non-zero terms of the same order have equal coefficients.
  ; Terms that equal zero will be ignored.
  ; This assumes polys are sorted by term order.
  
  (define (equal-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (equal-terms? (term-list p1) (term-list p2))
        )
    )
  
  (define (equal-terms? tl1 tl2)
    (cond 
      ; if either is empty, check if the other is equal to zero.
      ((empty-termlist? tl1) (equal-zero? tl2))  
      ((empty-termlist? tl2) (equal-zero? tl1))
      (else
       (let ((t1 (first-term tl1))
             (t2 (first-term tl2))
             )
         (if (equ? (order t1) (order t2))
             (and (equ? (coeff t1) (coeff t2)) (equal-terms? (rest-terms tl1) (rest-terms tl2)))
             (cond 
               ((=zero? (coeff t1)) (equal-terms? (rest-terms tl1) tl2))
               ((=zero? (coeff t2)) (equal-terms? tl1 (rest-terms tl2)))
               (else
                false
                )
               )
             )
         )
       )
      )
    )
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))  ; add-poly could handle this, but it'd be an odd message
        (add-poly p1
                  (mul-poly p2
                            (make-poly (variable p2) '((0 -1)))
                            )
                  )
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2)
               )
        )
    )
  
  ; interface
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2)))
       )
  
  ; added equ for polys
  (put 'equ? '(polynomial polynomial)
       equal-poly
       )
  
  'done
  )

(update-polynomial-package)
; Testing

; For testing, we need a means of checking for polynomial equality:
; Be sure to install these routines or something similar in the polynomial package.

; The definition of equality used here:
; Polynomials are equal if they are in the same variable, and all non-zero terms of the same order have equal coefficients.
; (It's implicit in the polynomial representation that each order has at most one term).

;



(header "Polynomial Testing: subtraction")

; Testing polynomial operations.

; Polynomial binary op tests all follow the same basic format. Each test is given an input list and an expected results list.

; The input list requires 7 polynomials. All but the last must be in the same variable.
; It uses the labels poly-0 through poly-4 for the first 5.  
; poly-0 must =zero?
; poly-1 through poly-4 can have any values; see the results list for how they are used.
; The last two are poly-id and poly-y.  
; poly-id is an identity, such that any-poly op poly-id = any-poly (as long as it's in the same variable)
; poly-y is in a different variable than the first six polys.

; The results list contains the following results in order, for the given binary operation 'op':
; (poly-2 op poly-1)
; (poly-4 op poly-3)
; (poly-2 op poly-4)


; Inputs for addition and subtraction tests
(define input-list (list (make-polynomial 'x '((0 0)))                 ; zero
                         (make-polynomial 'x '((1 1)))                 ; poly-1
                         (make-polynomial 'x '((1 5) (0 3)))           ; poly-2
                         (make-polynomial 'x '((3 6.4) (2 -5) (1 -4))) ; poly-3
                         (make-polynomial 'x '((4 -1) (3 1) (2 2) (1 5) (0 -3)))  ; poly-4
                         (make-polynomial 'x '((0 0)))                 ; identity in x
                         (make-polynomial 'y '((2 2) (1 4) (0 8)))     ; poly in another variable
                         ) 
  )

; Inputs for multiplication test
(define mul-input-list (list (make-polynomial 'x '((0 0)) )               ; zero
                             (make-polynomial 'x '((2 -3) (1 2)) )        ; poly-1
                             (make-polynomial 'x '((2 -3) (1 2)) )        ; poly-2
                             (make-polynomial 'x '((3 6.4) (2 1) (0 1)) ) ; poly-3
                             (make-polynomial 'x '((4 13)  (2 -5) (1 6) (0 4)) )  ; poly-4
                             (make-polynomial 'x '((0 1)) )               ; identity in x
                             (make-polynomial 't '((2 2) (1 4) (0 8)) )   ; poly in another variable
                             ) 
  )

; Run the tests
(poly-add-test input-list 
               (list (make-polynomial 'x '((1 6) (0 3)) )  ; poly-2 + poly-1
                     (make-polynomial 'x '((4 -1) (3 7.4) (2 -3) (1 1) (0 -3)) ) ; poly-4 + poly-3
                     (make-polynomial 'x '((4 -1) (3 1) (2 2) (1 10)) ) ; poly-2 + poly-4
                     )
               )

(poly-sub-test input-list 
               (list (make-polynomial 'x '((1 4) (0 3)) )  ; poly-2 - poly-1
                     (make-polynomial 'x '((4 -1) (3 -5.4) (2 7) (1 9) (0 -3))) ; poly-4 - poly-3
                     (make-polynomial 'x '((4 1) (3 -1) (2 -2) (0 6))) ; poly-2 - poly-4
                     )
               )

(poly-mul-test mul-input-list 
               (list (make-polynomial 'x '((4 9) (3 -12) (2 4)) ) ; poly-2 * poly-1
                     (make-polynomial 'x '((7 83.2) (6 13) (5 -32) (4 46.4) (3 31.6) (2 -1) (1 6) (0 4)) ) ; poly-4 * poly-3
                     (make-polynomial 'x '((6 -39) (5 26) (4 15) (3 -28) (1 8))) ; poly-2 * poly-4
                     )
               )

; Ex 2.89.
; Implementing terms for dense polynomials

; Implement the procedures necessary to store terms of
; dense polynomials.

(define (add-n-leading-s n s li)
  (if (<= n 0)
      li
      (add-n-leading-s (- n 1) s (cons s li))
      )
  )

(define (add-leading-zeros n li)
  (add-n-leading-s n 0 li)
  )

(define (add-trailing-zeros n li)
  (append li (add-n-leading-s n 0 '()))
  )

; A list approach instead of an individual term approach

(define (install-dense-polynomial-package)
  
  ;; internal procedures
  ; unchanged 
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  (define (the-empty-termlist) '())
  
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  ; Order is now applied to term-lists, not individual terms
  (define (order term-list) (- (length term-list) 1))
  (define (first-coeff term-list)
    (car term-list)
    )
  
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
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))  ; add-poly could handle this, but it'd be an odd message
        (add-poly p1
                  (mul-poly p2
                            (make-poly (variable p2) '(-1))
                            )
                  )
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2)
               )
        )
    )
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-coeff L1) (order L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  ; 
  ; equal-poly has no need to change, but equal-terms? must be defined for dense polys
  (define (equal-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (equal-terms? (term-list p1) (term-list p2))
        )
    )
  
  ; modified for dense polys
  
  
  (define (add-terms tl1 tl2)
    (let ((order-diff (- (order tl1) (order tl2)))
          )
      (if (< order-diff 0)
          (map add (add-leading-zeros (abs order-diff) tl1) tl2)
          (map add tl1 (add-leading-zeros order-diff tl2))       ; handles order-diff=0
          )
      )
    )
  
  
  (define (mul-term-by-all-terms t1 t1-order L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (add-trailing-zeros t1-order
                            (map (lambda(x) (mul t1 x)) L)
                            )
        )
    )
  
  
  
  ; check if all terms are =zero
  (define (equal-zero? term-list)
    (if (empty-termlist? term-list)
        true
        (and (=zero? (first-coeff term-list)) (equal-zero? (rest-terms term-list)))
        )
    )
  
  (define (equal-terms? tl1 tl2)
    (cond
      ; if either list is empty, check that the rest of the other is empty or zero
      ((empty-termlist? tl1) (equal-zero? tl2)) 
      ((empty-termlist? tl2) (equal-zero? tl1))
      ; if the leading coefficient in one list is zero, it can be skipped (the other list does not require a matching term)
      ((=zero? (first-coeff tl1)) (equal-terms? (cdr tl1) tl2))
      ((=zero? (first-coeff tl2)) (equal-terms? tl1 (cdr tl2)))
      ; otherwise compare terms
      ((equ? (first-coeff tl1) (first-coeff tl2)) (equal-terms? (rest-terms tl1) (rest-terms tl2)))
      ;
      (else 
       false
       )
      )
    )
  
  ;; interface to rest of the system [unchanged except for tags]
  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense) 
       (lambda (p1 p2) (tag (add-poly p1 p2)))
       )
  (put 'mul '(dense dense) 
       (lambda (p1 p2) (tag (mul-poly p1 p2)))
       )
  (put 'sub '(dense dense)
       (lambda (p1 p2) (tag (sub-poly p1 p2)))
       )
  
  (put 'equ? '(dense dense)
       equal-poly
       )
  (put '=zero? '(dense) 
       (lambda (p) (equal-zero? (term-list p)))
       )
  
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  )

; Testing/demonstration 
(install-dense-polynomial-package)

(define (make-dense-poly var terms) ((get 'make 'dense) var terms))

; Testing
(header "Testing Polynomials: dense polynomials")

(define dense-input-list (list (make-dense-poly 'x '(0) )   ; zero
                               (make-dense-poly 'x '(1 0) )   ; poly-1
                               (make-dense-poly 'x '(5 3) )  ; poly-2
                               (make-dense-poly 'x '(6.4 -5 -4 0) ) ; poly-3
                               (make-dense-poly 'x '(-1 1 2 5 -3) )  ; poly-4
                               (make-dense-poly 'x '(0))    ; poly-id (addition)
                               (make-dense-poly 'y '(2 4 8) )  ;poly in another variable
                               ) 
  )                                      
 

(define dense-mul-input-list (list (make-dense-poly 'x '(0) )    ; zero
                                   (make-dense-poly 'x '(3 2 0) )   ; poly-1
                                   (make-dense-poly 'x '(3 -2 0) )  ; poly-2
                                   (make-dense-poly 'x '(6.4 1 0 1) ) ; poly-3
                                   (make-dense-poly 'x '(13 0 -5 6 4) )  ; poly-4
                                   (make-dense-poly 'x '(1) )   ; poly-id
                                   (make-dense-poly 'y '(2 4 8) )  ;poly in another variable
                                   ) 
  )

(poly-add-test dense-input-list 
               (list 
                (make-dense-poly 'x '(6 3))  ; poly-2 + poly-1
                (make-dense-poly 'x '(-1 7.4 -3 1 -3))  ; poly-4 + poly-3
                (make-dense-poly 'x '(-1 1 2 10 0))  ; poly-2 + poly-4
                )
               )

(poly-mul-test dense-mul-input-list 
               (list
                (make-dense-poly 'x '(9 0 -4 0 0))   ; poly-2 * poly-1
                (make-dense-poly 'x '(83.2 13 -32 46.4 31.6 -1 6 4))  ; poly-4 * poly-3
                (make-dense-poly 'x '(39 -26 -15 28 0 -8 0)) ; poly-2 * poly-4
                )
               )

(poly-sub-test dense-input-list
               (list (make-dense-poly 'x '(4 3) )  ; poly-2 - poly-1
                     (make-dense-poly 'x '(-1 -5.4 7 9 -3) ) ; poly-4 - poly-3
                     (make-dense-poly 'x '(1 -1 -2 0 6) ) ; poly-2 - poly-4
                     )
               )

; Ex 2.90.
; Implementing both sparse and dense polys

; Make necessary changes so that either the sparse or the dense polynomial representation can be used.

; Sparse polys (nearly identical to original poly package)

(define (install-sparse-poly-package)
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
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1
                  (mul-poly p2
                            (make-poly (variable p2) '((0 -1)))
                            )
                  )
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2)
               )
        )
    )
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  ; determines if all terms are =zero?
  (define (equal-zero? term-list)
    (if (empty-termlist? term-list)
        true
        (and (=zero? (coeff (first-term term-list))) (equal-zero? (rest-terms term-list)))
        )
    )
  
  ; Polynomials are equal if they are the same variable, and have terms of the same order whose coefficients are equal.
  ; This assumes polys with terms sorted high to low.
  (define (equal-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (equal-terms? (term-list p1) (term-list p2))
        )
    )
  
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
  
  (define (equal-terms? tl1 tl2)
    (cond 
      ((empty-termlist? tl1) (equal-zero? tl2))  
      ((empty-termlist? tl2) (equal-zero? tl1))
      (else
       (let ((t1 (first-term tl1))
             (t2 (first-term tl2))
             )
         (if (= (order t1) (order t2))
             (and (equ? (coeff t1) (coeff t2)) (equal-terms? (rest-terms tl1) (rest-terms tl2)))
             (cond 
               ((=zero? (coeff t1)) (equal-terms? (rest-terms tl1) tl2))
               ((=zero? (coeff t2)) (equal-terms? tl1 (rest-terms tl2)))
               (else
                false
                )
               )
             )
         )
       )
      )
    )
  
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(sparse sparse)
       (lambda (p1 p2) (tag (sub-poly p1 p2)))
       )
  (put 'mul '(sparse sparse) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  
  (put '=zero? '(sparse) 
       (lambda (p) (equal-zero? (term-list p)))
       )
  (put 'equ? '(sparse sparse)
       equal-poly
       )
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))

  (put 'poly-variable '(sparse)
       (lambda (sp) (variable sp))
       )
  
  'done
  )


; Dense-poly procedures


(define (update-dense-poly-package)
  
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  (define (the-empty-termlist) '())
  
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (order term-list) (- (length term-list) 1))
  (define (first-coeff term-list)
    (car term-list)
    )
  
  ; Changes to work with sparse polys
  (define (make-sparse-poly var terms) ((get 'make 'sparse) var terms))
  (define sparse-poly-empty-termlist (the-empty-termlist))
  
  ; Used for conversion to sparse poly
  (define (make-pair-list term-list)
    (if (empty-termlist? term-list)
        sparse-poly-empty-termlist
        (cons (list (order term-list)
                    (first-coeff term-list)
          )
              (make-pair-list (rest-terms term-list))
              )
        )
    )
  
  ;; interface to rest of the system [unchanged]
  (put 'raise 'dense ; to sparse
       (lambda (dp) (make-sparse-poly (variable dp)
                                      (make-pair-list (term-list dp))
                                      )
         )
       )

  (put 'poly-variable '(dense)
       (lambda (dp) (variable dp))
       )
  )

; Use precedence to make them compatible with each other.  While the choice of precedence is arbitrary, in this set-up raise is only defined for dense polys, so this order must not be changed.
(set-precedence 'dense 2)
(set-precedence 'sparse 1)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  
  (define (make-sparse-poly var terms) ((get 'make 'sparse) var terms))
  (define (make-dense-poly var terms) ((get 'make 'dense) var terms))
  
  (define (make-poly var terms)
    ; decide what to do based on term-list
    (cond
      ((null? terms) (make-sparse-poly var terms))
      ((pair? (car terms)) (make-sparse-poly var terms))
      (else (make-dense-poly var terms))
      )
    )
  
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
    )
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add p1 p2)))
       )
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul p1 p2)))
       )
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub p1 p2)))
       )
  (put '=zero? '(polynomial)
       (lambda (p) (=zero? p))
       )

  ; Attempt to combine with other values


  (define (add-constant-to-poly c p1)
    (add p1 (make-poly (poly-variable p1) (list (list 0 c))))
    )

  (define (mul-constant-by-poly c p1)
    (mul p1 (make-poly (poly-variable p1) (list (list 0 c))))
    )

  (put 'add '(polynomial complex)
     (lambda (p c) (tag (add-constant-to-poly (attach-tag 'complex c) p)))
     )

  (put 'add '(complex polynomial)
       (lambda (c p) (tag (add-constant-to-poly (attach-tag 'complex c) p)))
       )

  (put 'mul '(polynomial complex)
     (lambda (p c) (tag (mul-constant-by-poly (attach-tag 'complex c) p)))
     )
  
  (put 'mul '(complex polynomial)
        (lambda (c p) (tag (mul-constant-by-poly (attach-tag 'complex c) p)))
     )
  
  ; added equ for polys
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2) (equ? p1 p2))
       )

  (put 'poly-variable '(polynomial)
       (lambda (p1) (poly-variable p1))
       )
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))
       )
  'done
  )

(define (poly-variable p)
  (apply-generic 'poly-variable p)
  )

; Testing

(newline)
(display "Installing Sparse-poly package ...")
(install-sparse-poly-package)
(display "Installing Dense-poly package ...")
(update-dense-poly-package)
(display "Installing Polynomial package ...")
(install-polynomial-package)
(header "Testing Polynomials: mixed sparse & dense")

; This also checks the basic ability to make polynomials, and sparse polys
(define sp-0 (make-polynomial 'x '((0 0)) ))   ; 0
(define sp-1 (make-polynomial 'x '((2 1)) ))   ; x^2
(define sp-2 (make-polynomial 'x '((1 1)) ))  ; x^1
(define sp-3 (make-polynomial 'x '((100 1) (2 2) (0 1)) )) ; x^100 + 2x^2 + 1
(define sp-4 (make-polynomial 'x '((3 1) (2 -2) (1 3) (0 5)) )) ; x^3 - 2x^2 + 3x + 5
(define sp-mi (make-polynomial 'x '((0 1)) ))  ; 1
(define sp-y (make-polynomial 'y '((100 1) (2 2) (0 1)) )) ; y^100 + 2y^2 + 1
(displayln "Sparse polys created.")

(displayln "Testing dense polynomials")

; This tests the ability to make dense polynomials
(define dp-0 (make-polynomial 'x '(0)))      ; 0
(define dp-1 (make-polynomial 'x '(1 0 0)))  ; x^2
(define dp-2 (make-polynomial 'x '(0 1 2)))  ; x + 2
(define dp-3 (make-polynomial 'x '(1 2 0 3 -2 -5))) ; x^5 + 2x^4 + 3x^2 - 2x - 5
(define dp-4 (make-polynomial 'x '(1 -2 3 5))) ; x^3 - 2x^2 + 3x + 5
(define dp-mi (make-polynomial 'x '(1)))     ; 1
(define dp-y (make-polynomial 'y '(1 2 0 3 -2 -5))) ; y^5 + 2y^4 + 3y^2 - 2y - 5

(displayln "Dense polys created.")

(header "Math tests using sparse polys")
(poly-add-test (list sp-0 sp-1 sp-2 sp-3 sp-4 sp-0 sp-y) 
               (list 
                (make-polynomial 'x '((2 1) (1 1)) ) ; poly-2 + poly-1
                (make-polynomial 'x '((100 1) (3 1) (1 3) (0 6)) ) ; poly-3 + poly-4
                (make-polynomial 'x '((3 1) (2 -2) (1 4) (0 5)) ) ;poly-2 + poly-4
                )
               )

(newline)
(poly-sub-test (list sp-0 sp-1 sp-2 sp-3 sp-4 sp-0 sp-y) 
               (list
                (make-polynomial 'x '((2 -1) (1 1)) ) ; poly-2 - poly-1
                (make-polynomial 'x '((100 -1) (3 1) (2 -4) (1 3) (0 4)) ) ; poly-4 - poly-3
                (make-polynomial 'x '((3 -1) (2 2) (1 -2) (0 -5)) ) ; poly-2 - poly-4
                )
               )
(newline)
(poly-mul-test (list sp-0 sp-1 sp-2 sp-3 sp-4 sp-mi sp-y)
               (list 
                (make-polynomial 'x '((3 1)) ) ; poly-2 * poly-1
                (make-polynomial 'x '((103 1) (102 -2) (101 3) (100 5) (5 2) (4 -4) (3 7) (2 8) (1 3) (0 5)) ) ; poly-3 * poly-4
                (make-polynomial 'x '((4 1) (3 -2) (2 3) (1 5)) ) ;poly-2 * poly-4
                )
               )

(header "Zero Tests (sparse polys)")
(poly-zero-test sp-0 sp-1 (make-polynomial'x '((2 -1)))) 
(newline)

(header "Math tests using dense polys")
(poly-add-test (list dp-0 dp-1 dp-2 dp-3 dp-4 dp-0 dp-y) 
               (list (make-polynomial 'x '(1 1 2) ) ; poly-1 + poly-2
                     (make-polynomial 'x '(1 2 1 1 1 0) ) ; poly-3 + poly-4
                     (make-polynomial 'x '(1 -2 4 7) ) ;poly-2 + poly-4
                     )
               )

(newline)
(poly-sub-test (list dp-0 dp-1 dp-2 dp-3 dp-4 dp-0 dp-y) 
               (list (make-polynomial 'x '(-1 1 2) ) ; poly-2 - poly-1
                     (make-polynomial 'x '(-1 -2 1 -5 5 10) ) ; poly-4 - poly-3
                     (make-polynomial 'x '(-1 2 -2 -3) ) ; poly-2 - poly-4
                     )
               )

(newline)
(poly-mul-test (list dp-0 dp-1 dp-2 dp-3 dp-4 dp-mi dp-y)
               (list (make-polynomial 'x '(1 2 0 0) ) ; poly-2 * poly-1
                     (make-polynomial 'x '(1 0 -1 14 2 8 19 -25 -25) ) ; poly-3 * poly-4
                     (make-polynomial 'x '(1 0 -1 11 10) ) ;poly-2 * poly-4
                     ))

(newline)
(displayln "Zero Tests (dense polys)")
(poly-zero-test dp-0 dp-1 (make-polynomial 'x '(-1 0 0))) 

(header "Math tests with mixed polynomials")
; Some of these tests may depend on how equality functions.

(poly-add-test (list dp-0 sp-1 dp-2 sp-3 dp-4 sp-0 dp-y) 
               (list (make-polynomial 'x '(1 1 2) )  ; sp-2 + dp-1
                     (make-polynomial 'x '((100 1) (3 1) (1 3) (0 6)) )  ; sp-4 + dp-3
                     (make-polynomial 'x '((3 1) (2 -2) (1 4) (0 7)) )  ; dp-2 + dp-4
                     )
               )

(poly-sub-test (list sp-0 dp-1 sp-2 dp-3 sp-4 dp-0 sp-y) 
               (list (make-polynomial 'x '((2 -1) (1 1)) )      ; sp-2 - dp-1
                     (make-polynomial 'x '(-1 -2 1 -5 5 10) )      ; sp-4 - dp-3
                     (make-polynomial 'x '(-1 2 -2 -5) )      ; sp-2 - sp-4
                     )
               )

(poly-mul-test (list sp-0 dp-1 sp-2 dp-3 dp-4 dp-mi dp-y) 
               (list (make-polynomial 'x '(1 0 0 0) )      ; sp-2 * dp-1
                     (make-polynomial 'x '((8 1) (6 -1) (5 14) (4 2) (3 8) (2 19) (1 -25) (0 -25)) )      ; dp-4 * dp-3
                     (make-polynomial 'x '(1 -2 3 5 0) )      ; sp-2 * dp-4
                     )
               )

(header "Zero tests (mixed)")
(poly-zero-test dp-0 (make-polynomial 'x '((2 1))) (make-polynomial 'x '(-1 0 0)))
(poly-zero-test sp-0 (make-polynomial 'x '(3 -2 0 1)) (make-polynomial 'x '((3 -3) (2 2) (0 -1))))
(newline)

; Ex 2.91
; Polynomial division

; Complete the 'div-terms' procedure and implement div-poly to perform
; polynomial division (with quotient and remainder).

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
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1
                  (mul-poly p2
                            (make-poly (variable p2) '((0 -1)))
                            )
                  )
        (error "Polys not in same var -- SUB-POLY"
               (list (p1 p2))
               )
        )
    )
  
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
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-result (div-terms (term-list p1)
                                     (term-list p2)
                                     (variable p2)
                                     )
                          )
              )
          (list (make-poly (variable p1) (quotient div-result))
                (make-poly (variable p2) (remainder div-result))
                )
          )
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))
        )
    )
  
  (define (div-terms L1 L2 var)
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
                       (div-terms (term-list (sub-poly (make-poly var L1)
                                                       (make-poly var (mul-terms L2
                                                                                 (list (make-term new-o new-c)))
                                                                  )
                                                       )
                                             )
                                  L2
                                  var
                                  )
                       )
                      )
                  (list (cons (make-term new-o new-c) (quotient rest-of-result))
                        (remainder rest-of-result)
                        )
                  )
                )
              )
          )
        )
    )
  
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  
  (put 'div '(sparse sparse)
       (lambda (p1 p2) 
         (let ((div-res (div-poly p1 p2)))
           (attach-tag 'div-result                          ; A tag is required to test for dropping
                       (list (tag (quotient div-res))
                             (tag (remainder div-res))
                             )
                       )
           )
         )
       )
  
  'done
  )

(define (update-polynomial-package)
  (define (quotient result) (car result))
  
  (define (remainder result) (cadr result))
  
  (define (tag p) (attach-tag 'polynomial p))
  
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (let ((div-result (cdr (div p1 p2)))) ; temp remove tag
           (cons 'div-result
                 (list (tag (quotient div-result))
                       (tag (remainder div-result))
                       )
                 )
           )
         )
       )
  
  )

(define (poly-quotient div-result)
  (cadr div-result)
  )

(define (poly-remainder div-result)
  (caddr div-result)
  )

(display "Updating sparse-poly package...")
(update-sparse-poly-package)
(display "Updating polynomial package...")
(update-polynomial-package)

; Testing division
(displayln "Testing Polynomials: division")

(define poly-4 (make-polynomial 'x '((2 3) (1 -2) (0 1)) ))

(define input-list (list sp-0    ; zero
                         dp-1    ; poly-1
                         (make-polynomial 'x '((6 -39) (5 26) (4 15) (3 -28) (1 8)) ) ; poly-2
                         sp-3    ; poly-3
                         poly-4
                         dp-mi   ; id
                         (make-polynomial 'y '((2 2) (1 4) (0 8)))  ;poly in another variable
                         )
  )

(poly-div-test input-list 
               (list (make-polynomial 'x '((4 -39) (3 26) (2 15) (1 -28))  )      ; quotient - poly-2/poly-1
                     (make-polynomial 'x '((1 8)) )                               ; remainder 
                     sp-0                                                         ; quotient - poly-4/poly-3
                     poly-4                                                       ; remainder 
                     (make-polynomial 'x (list (list 4 -13) (list 2 (make-rational 28 3)) (list 1 (make-rational -28 9)) (list 0 (make-rational -140 27))) ) ; quotient - poly-2/poly-4
                     (make-polynomial 'x (list (list 1 (make-rational 20 27)) (list 0 (make-rational 140 27))) ) ; remainder
                     )
               )

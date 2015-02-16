; Section 2.3.2

; Deriv 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (augend exp) var)
                   (deriv (addend exp) var)
                   )
         )
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var)
                        )
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)
                        )
          )
         )
        (else (error "unknown expression type -- DERIV" exp))
        )
  )

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (augend s) (cadr s))

(define (addend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))
        )
  )

(define (=number? exp num)
  (and (number? exp) (= exp num)
       )
  )

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
        )
  )

; Testing
; Demonstrating basic derivative function
(define (basic-deriv-check)
  (displayln "Testing basic derivatives:")
  (displayln (deriv '(+ x 3) 'x))  ; 1
  (displayln (deriv '(* x y) 'x))  ; y
  (displayln (deriv '(* (* x y) (+ x 3)) 'x)) ; (2xy + 3y)
  (displayln "done.")
  )

; Verifying Derivatives
(displayln "Verifying initial derivative operation.")
(basic-deriv-check)

; Ex. 2.56.
; Extending differentiation [to powers]


; Testing
(displayln "Testing modified derivative")
(basic-deriv-check)

(define (pow-deriv-check) 
  (displayln "Testing power rule for derivatives:")
  (displayln (deriv '(<pow-op> x  2) 'x))    ; 2x
  (displayln (deriv '(<pow-op> a  3) 'a))    ; 3a^2
  (displayln (deriv '(<pow-op x -1) 'x))   ; -1/(x^2)
  (displayln (deriv '(<pow-op> x  1) 'x))    ; 1
  (displayln (deriv '(<pow-op> x  0) 'x))    ; 0
  (displayln (deriv '(<pow-op> x  4) 'a))    ; 0
  (displayln "done.")
  )

(pow-deriv-check)

; more complex expressions
; (x + 5)^4.  d/dx = 4(x+5)^3
(deriv '(<pow-op> (+ x 5) 4) 'x)  
; (3x^2 + x)^5. d/dx = 5*(6x + 1)*(3x^2 +x)^4
(deriv '(<pow-op> (+ (* 3 (<pow-op> x 2)) x) 5) 'x)


; Ex 2.57.
; Handling arbitrary-length terms


; Testing
(newline)
(displayln "Testing arbitrary lists for derivatives")

; Testing make-sum and make-product
(displayln "Testing make-sum and make-product")
(make-sum 2 2)
(make-sum 'a 'b)
(make-sum 'a 'b 'c 'd)
(make-sum 1 2 4)  ; 7 
(make-product 5 5) ; 25
(make-product 'a 'x)
(make-product 'a 'b 'c 'd)
(make-product 4 5 6)   ; 120
(make-product 5 2 'x) ; 10x

; Extra Testing
(make-sum '(+ 2 x) 'y)
(make-sum 'y '(+ 2 x))
(make-sum 'x 1 2)
(make-sum 3 'a 5)
(make-product 'x 3 4)
(make-product 5 '(* (* 5 x) y z))
(make-product 5 '(* z y (* 5 x)))
(make-sum (make-product 2 3) 7 (make-product 6 'x) (make-product 3 'x) )

; no specified behavior
(augend '(+ 2 3))
(augend '(+ 2 4 6))
(addend '(+ 2 3))
(addend '(+ 2 4 6))
(multiplier '(* 3 4))
(multiplicand '(* 3 4))
(multiplier '(* 2 3 4))
(multiplicand '(* 2 3 4))

; Testing derivatives
(displayln "Checking derivatives")
(basic-deriv-check) 
;(pow-deriv-check)  ; optional, but ought to work fine
(deriv '(* x y (+ x 3)) 'x)  ; book example = y(x+3) + xy
(define quadratic '(+ (* a (<pow-op> x 2)) (* b x) c))
(deriv quadratic 'x)  ; 2ax + b
(deriv quadratic 'a)  ; x^2
(deriv quadratic 'b)  ; x
(deriv quadratic 'c)  ; 1
(deriv '(<pow-op> (+ (* 3 (<pow-op> x 2)) x) 5) 'x) ; 5(6x + 1)(3x^2 + x)^4

; Ex 2.58
; Using infix notation

; a. Modify the program to work with infix, only two terms, 
; fully parenthesized.


; Testing

; See also (calc-xpr) below
(displayln "Testing infix operation (two terms only)")

(define (basic-constructor-selectors-check)
  (displayln "Testing constructors")
  (displayln  (make-sum 'a 'b))
  (displayln  (make-sum 'a '(b + c) ))
  
  (displayln  (make-product 'a 'b))
  (displayln  (make-product 'a '(b * c) ))
  (displayln  (make-product (make-sum 'a 'b) (make-sum 'c 'd)))
  
  (displayln "Testing selectors")
  (displayln  (equal? 'a (augend '(a + b))))
  (displayln  (equal? 'b (addend '(a + b))))
  
  (displayln  (equal? 'a (multiplier '(a * b))))
  (displayln  (equal? 'b (multiplicand '(a * b))))
)

(basic-constructor-selectors-check)

(make-sum 'a 'b 'c) ; what happens for this?

; redefine tests
(define (infix-basic-deriv-check)
  (displayln "Testing derivatives with infix operators")
  (displayln (deriv '(x + 3) 'x))    ; 1
  (displayln (deriv '(x * y) 'x))    ; y
  (displayln (deriv '((x * y) * (x + 3)) 'x))        ; (2xy + 3y)
  (displayln (deriv '(x + (3 * (x + (y + 2)))) 'x))  ; 4
  (displayln "done.")
  )

(infix-basic-deriv-check)  

; b. (A harder problem) Make this work using normal algebraic
; operator precedence, with unnecessary parentheses dropped.




; Testing expressions
(newline)  
(displayln "Testing infix notation with precedence")

(basic-constructor-selectors-check)

(displayln "More constructor & selector testing")
(make-sum 'a 'b 'c)
(make-sum 'a '(b + c) 'd)
(make-product 'a 'b 'c)
(make-product 'a '(b * c) 'd)
(make-sum 'a 'b '* 'c 'd)
(make-sum 'a 'b '* 'c '* 'd 'e)
(make-product 'a '+ 'b 'c 'd)

(augend '(a + b + c))   
(addend '(a + b + c))
(multiplier '(a * b * c))
(multiplicand '(a * b * c))
(augend '(a + b * c))
(addend '(a + b * c))
(multiplier (addend '(a + b * c)))
(multiplicand (addend '(a + b * c)))

; This may be useful for testing infix expressions (without trying to do derivatives)
(define (calc-xpr exp)
  (cond ((number? exp) exp)
        ((variable? exp) exp)
        ((sum? exp)
         (make-sum (calc-xpr (augend exp))
                   (calc-xpr (addend exp))
                   )
         )
        ((product? exp)
         (make-product (calc-xpr(multiplier exp))
                       (calc-xpr(multiplicand exp))
                       )
         )
        ; include power rule if desired ...  
        (else (error "unknown expression type -- CALC-XPR" exp))
        )
  )

(define a 2)
(define b 3)
(define c 7)
(define d 11)
(define e 19)

(displayln "Testing calculation")
(calc-xpr (list a '+ b '+ c))  ; 12
(calc-xpr (list a '+ b '* c))  ; 23
(calc-xpr (list (list a '+ b) '* c)) ; 35
(calc-xpr (list a '* b '+ c))  ; 13
(calc-xpr (list a '+ (list b '+ c))) ; 12
(calc-xpr (list a '* b '+ c '* d)) ; 83
(calc-xpr (list a '* (list b '+ c) '* d)) ; 220
(calc-xpr (list a '+ b '* c '+ d)) ; 34
(calc-xpr (list a '+ b '* c '* (list d '+ e))) ; 632


(infix-basic-deriv-check)  



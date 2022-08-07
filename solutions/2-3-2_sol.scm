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
  (displayln "Checking basic derivatives:")
  (displayln (deriv '(+ x 3) 'x))  ; 1
  (displayln (deriv '(* x y) 'x))  ; y
  (displayln (deriv '(* (* x y) (+ x 3)) 'x)) ; (2xy + 3y)
  (displayln "done.")
  )

; Verifying Derivatives
(displayln "Verifying initial derivative operation.")
(basic-deriv-check)

; Ex. 2.56.
; Extending differentiation to powers

; Modified deriv 
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
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp))
               )
           (make-product (make-product n
                                       (make-exponentiation u (- n 1))
                                       )
                         (deriv u var)
                         )
           )
         )  
        (else (error "unknown expression type -- DERIV" exp))
        )
  )

; Procedures for exponentiation

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**))
  )

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponentiation base power)
  (cond ((=number? power 0) 1)
        ((=number? power 1) base)
        ((and (number? base) (number? power)) (expt base power))
        (else (list '** base power))
        )
  )
; Testing
(displayln "Verifying modified derivative")
(basic-deriv-check)

; New functions
(define (pow-deriv-check) 
  (displayln "Checking power rule for derivatives:")
  (displayln (deriv '(** x  2) 'x))    ; 2x
  (displayln (deriv '(** a  3) 'a))    ; 3a^2
  (displayln (deriv '(** x -1) 'x))   ; -1/(x^2)
  (displayln (deriv '(** x  1) 'x))    ; 1
  (displayln (deriv '(** x  0) 'x))    ; 0
  (displayln (deriv '(** x  4) 'a))    ; 0
  (displayln "done.")
  )

(pow-deriv-check)

; more complex expressions
; (x + 5)^4.  d/dx = 4(x+5)^3
(deriv '(** (+ x 5) 4) 'x)  
; (3x^2 + x)^5. d/dx = 5*(6x + 1)*(3x^2 +x)^4
(deriv '(** (+ (* 3 (** x 2)) x) 5) 'x)


; Ex 2.57.
; Handling arbitrary-length terms

(define (addend exp)
  (make-sum-with-list (caddr exp) (cdddr exp))
  )

(define (make-sum-with-list a1 li)
  (if (null? li)
      a1
      (let ((a2 (car li))
            (rest-terms (cdr li))
            )
        (display a1
                 )
        (display a2)
        (cond
          ((=number? a1 0) (make-sum-with-list a2 rest-terms))
          ((=number? a2 0) (make-sum-with-list a1 rest-terms))
          ((and (number? a1) (number? a2)) (make-sum-with-list (+ a1 a2) rest-terms))
          ((sum? a1) (make-sum-with-list a2 (append (cdr a1) rest-terms))) ; extract the summing terms of a1
          ((sum? a2) (make-sum-with-list a1 (append (cdr a2) rest-terms))) ; likewise with a2
          (else (append (list '+ a1 a2) rest-terms))
          )
        )
      )
  )

(define (make-sum aug . adds) 
  (cond
    ((null? adds) aug) 
    (else (make-sum-with-list aug adds))
    )
  )

(define (multiplicand exp)
  (make-product-with-list (caddr exp) (cdddr exp))
  )

(define (make-product-with-list m1 li)
  (if (null? li)
      m1
      (let ((m2 (car li))
            (rest-terms (cdr li))
            )
        (cond
          ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) (make-product-with-list m2 rest-terms))
          ((=number? m2 1) (make-product-with-list m1 rest-terms))
          ((and (number? m1) (number? m2)) (make-product-with-list (* m1 m2) rest-terms))
          ((product? m1) (make-product-with-list m2 (append (cdr m1) rest-terms)))
          ((product? m2) (make-product-with-list m1 (append (cdr m2) rest-terms)))
          (else (append (list '* m1 m2) rest-terms))
          
          )
        )
      )
  )


(define (make-product multp . multis) 
  (cond
    ((null? multis) multp) 
    (else (make-product-with-list multp multis))
    )
  )

; Testing
(newline)
(displayln "Verifying arbitrary lists for derivatives")

; Testing make-sum and make-product
(displayln "Verifying make-sum and make-product")
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
(pow-deriv-check)
(deriv '(* x y (+ x 3)) 'x)  ; book example = y(x+3) + xy
(define quadratic '(+ (* a (** x 2)) (* b x) c))
(deriv quadratic 'x)  ; 2ax + b
(deriv quadratic 'a)  ; x^2
(deriv quadratic 'b)  ; x
(deriv quadratic 'c)  ; 1
(deriv '(** (+ (* 3 (** x 2)) x) 5) 'x) ; 5(6x + 1)(3x^2 + x)^4

; Ex 2.58
; Using infix notation

; a. Modify the program to work with infix, only two terms, 
; fully parenthesized.
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (augend s) (car s))

(define (addend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))
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
        (else (list m1 '* m2))
        )
  )

; Testing

; See also (calc-xpr) below
(displayln "Verifying infix operation (two terms only)")

(define (basic-constructor-selectors-check)
  (displayln "Checking constructors")
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

;(make-sum 'a 'b 'c)  ; not allowed (causes arity error)

; redefine tests
(define (infix-basic-deriv-check)
  (displayln "Checking derivatives with infix operators")
  (displayln (deriv '(x + 3) 'x))    ; 1
  (displayln (deriv '(x * y) 'x))    ; y
  (displayln (deriv '((x * y) * (x + 3)) 'x))        ; (2xy + 3y)
  (displayln (deriv '(x + (3 * (x + (y + 2)))) 'x))  ; 4
  (displayln "done.")
  )

(infix-basic-deriv-check)  

; b. (A harder problem) Make this work using normal algebraic
; operator precedence, with unnecessary parentheses dropped.

; For this implementation, whitespace between symbols
; will be necessary.  (This is not in conflict with the
; example given, so I'm going with it).

; This returns straight numeric
; values for precedence.
(define nullop 'nullop)

(define (prec op)
  (cond
    ((eq? op '*) 1) 
    ((eq? op '+) 2)
    ((eq? op nullop) 99) ; should always be lowest precedence
    )
  )

(define (operator? item)
  (and (symbol? item) 
       (or
        (eq? item '*)
        (eq? item '+)
        )
       )
  )


; This will return true if op1 has higher precedence
(define (higher-prec? op1 op2)
  (< (prec op1) (prec op2))
  )

; Generic function for moving through an 
; expression that has operators of
; potentially higher precedence

(define (op-finder comp-op on-find)
  (define (op-down exp)
    (if (null? (cdr exp))  ; binary ops only for now
        (on-find nullop '())    
        (let ((next-op (cadr exp)))
          (if (higher-prec? next-op comp-op) 
              (let ((chunky-list (next-chunk next-op (cddr exp))))   
                (op-down (cons (list (car exp) next-op (car chunky-list)) (cdr chunky-list)))
                )
              (on-find next-op exp)
              )
          )
        )
    )
  (lambda (exp) (op-down exp))
  )

; This will get the next chunk in a list that this operator will work on.  It
; will return a list with the chunk as the car, and the remainder of the list
; passed.  It is similar to (op-finder), although that operates on expressions only.

(define (next-chunk op li)
  (cond 
    ((null? li) (error "no next item in list for operator:" op))
    ((null? (cdr li)) li)
    (else
     (let ((next-pot-op (cadr li)))
       (if (and (operator? next-pot-op)
                (higher-prec? next-pot-op op)
                )  
           (let ((hp-list (next-chunk next-pot-op (cddr li))))
             (next-chunk op (cons (list (car li) next-pot-op (car hp-list)) (cdr hp-list)))
             )
           li
           )
       )
     )
    )
  )

(define sum? 
  (op-finder '+ (lambda(next-op exp) (eq? next-op '+)))
  )

; These both assume the expression to be a sum
(define augend 
  (op-finder '+ (lambda(next-op exp) (car exp)))
  ) 

(define addend 
  (op-finder '+ 
             (lambda(next-op exp) 
               ;(cddr exp)
               (make-sum-with-list (cddr exp))            
               )
             )
  )

(define (make-binop-with-list binop make-binop li)
  (let ((la-list (next-chunk binop li)))
    (let ((la (car la-list))
          (la-rest (cdr la-list))
          )   
      (cond
        ((null? la-rest) la)
        ((operator? (car la-rest)) 
         (let ((next-op (car la-rest)))
           (cond 
             ((higher-prec? next-op binop) 
              (error "Can't process binary operation on " li)
              )
             ((eq? next-op binop)
              (let ((ra-list (next-chunk binop (cdr la-rest))))
                (make-binop la (car ra-list) (cdr ra-list)))
              )
             (else    ; op is lower prec. - add parentheses
              (let ((lower-chunk-list (next-chunk next-op (cdr la-rest))))
                (make-binop-with-list binop
                                      make-binop
                                      (append (list (list  la next-op (car lower-chunk-list))) (cdr lower-chunk-list))
                                      )
                )
              )
             )
           )
         ) 
        (else
         (let ((ra-list (next-chunk binop la-rest)))
           (make-binop la (car ra-list) (cdr ra-list))
           )
         )
        )
      )
    )
  )


(define (make-sum-with-list li)
  (make-binop-with-list '+ make-sum-of-two li)
  )

(define (make-sum-of-two a1 a2 rest-terms)
  (cond
    ((=number? a1 0) (make-sum-with-list (cons a2 rest-terms)))
    ((=number? a2 0) (make-sum-with-list (cons a1 rest-terms)))
    ((and (number? a1) (number? a2)) (make-sum-with-list (cons (+ a1 a2) rest-terms)))
    (else (list a1 '+  (make-sum-with-list (cons a2 rest-terms))))
    )
  )

; This is the same as used in Ex 2.57. 
(define (make-sum aug . adds) 
  (cond
    ((null? adds) aug) 
    (else (make-sum-with-list (cons aug adds)))
    )
  )

; Since the product operator has highest precedence,
; using op-finder isn't strictly necessary.

(define product? 
  (op-finder '* (lambda(next-op exp) (eq? next-op '*)))
  )

(define multiplier 
  (op-finder '* (lambda(next-op exp) (car exp)))
  ) 

(define multiplicand
  (op-finder '* 
             (lambda(next-op exp) 
               (make-product-with-list (cddr exp)) 
               )
             )
  )

(define (make-product-with-list li)
  (make-binop-with-list '* make-product-of-two li)
  )

(define (make-product-of-two m1 m2 rest-terms)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) (make-product-with-list (cons m2 rest-terms)))
    ((=number? m2 1) (make-product-with-list (cons m1 rest-terms)))
    ((and (number? m1) (number? m2)) (make-product-with-list (cons (* m1 m2) rest-terms)))
    (else (list m1 '* (make-product-with-list (cons m2 rest-terms))))
    )
  )

(define (make-product multp . multis) 
  (cond
    ((null? multis) multp) 
    (else (make-product-with-list (cons multp multis)))
    )
  )

; Testing expressions
(newline)  
(displayln "Verifying infix notation with precedence")

(basic-constructor-selectors-check)

(displayln "More constructor & selector verifying")
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
        
        ((exponentiation? exp)
         (make-exponentiation (calc-xpr(base exp)) (calc-xpr(exponent exp)))
         )  
        (else (error "unknown expression type -- CALC-XPR" exp))
        )
  )

(define a 2)
(define b 3)
(define c 7)
(define d 11)
(define e 19)

(displayln "Verifying calculations")
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

; Bonus operations
(define (prec op)
  (cond
    ((eq? op '**) 1)
    ((eq? op '*) 2) 
    ((eq? op '+) 3)
    ((eq? op nullop) 99) ; should always be lowest precedence
    )
  )

(define (operator? s)
  (and (symbol? s) 
       (or
        (eq? s '**)
        (eq? s '*)
        (eq? s '+)
        )
       )
  )


(define exponentiation? 
  (op-finder '** (lambda(next-op exp) (eq? next-op '**)))
  )

(define base
  (op-finder '** (lambda(next-op exp) (car exp)))
  ) 

(define exponent
  (op-finder '** 
             (lambda(next-op exp) 
               (make-exponent-with-list (cddr exp)) 
               )
             )
  )

(define (make-exponent-with-list li)
  (make-binop-with-list '** make-exponent-of-two li)
  )

(define (make-exponent-of-two b pow rest-terms)
  (if (not (null? rest-terms))
      (error "Chaining exponentiation is not allowed.  Extra terms are :" rest-terms)   
      (cond ((=number? pow 0) 1)
            ((=number? pow 1) b)
            ((and (number? b) (number? pow)) (expt b pow))
            (else (list b '** pow))
            )
      )
  )

(define (make-exponentiation base . powers) 
  (cond
    ((null? powers) base) 
    (else (make-exponent-with-list (cons base powers)))
    )
  )


; Testing exponentiation
(define (infix-pow-deriv-check)
  (displayln "Testing exponentiation (infix version)")
  (displayln (deriv '(x ** 2) 'x))    ; (2 * x)
  (displayln (deriv '(a ** 3) 'a))    ; (3 * (a ** 2))
  (displayln (deriv '(x ** -1) 'x))   ; (-1 * (x ** -2))
  (displayln (deriv '(x ** 1) 'x))    ; 1
  (displayln (deriv '(x ** 0) 'x))    ; 0
  (displayln (deriv '(x ** 4) 'a))    ; 0
  (displayln "done.")
  )

(infix-pow-deriv-check)
  
; more complex expressions
(displayln "Testing more complex derivatives")
(deriv '(a * x ** 2 + b * x + c) 'x)  ; quadratic. d/dx = 2ax + b 
(deriv '((x + 5) ** 4) 'x)  ; (x + 5)^4.  d/dx = 4(x+5)^3 
(deriv '(x + 5 ** 4) 'x) ; x + 5^4. d/dx = 1
(deriv '((x + 3 * x ** 2) ** 5) 'x) ; (3x^2 + x)^5. d/dx = 5*(6x + 1)*(3x^2 +x)^4

; These are both valid
(calc-xpr '(5 * a ** (3 ** 2)))
(calc-xpr '(5 * a ** 3 ** 2))
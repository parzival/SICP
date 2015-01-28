; Section 1.3.4

; Need to define if using Racket-derived languages
(define (inc x)
  (add1 x)  
  )

(define (square x)
  (sqr x)
  )

; Support procedures

(define tolerance 0.00001) 

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)
    )
  (define (try guess) 
    (let ((next (f guess)))
      (if (close-enough? guess next) next
          (try next))
      )
    )
  (try first-guess)
  )

(define (deriv g) 
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx))
  )

(define dx 0.00001)

(define (average x y)
  (/ (+ x y) 2)
  )

(define (newton-transform g) 
  (lambda (x)
    (- x (/ (g x) ((deriv g) x))))
  ) 

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess)
  )

; Ex 1.40
; Using Newton's Method solver

; Design a (cubic) procedure to work with the Newton's Method solver

; Testing 
(displayln "Testing cubic with Newton's Method solver")
(define (newtons-method-test)
  (displayln (newtons-method (cubic -3 -7 -15) 1.0))  ; 5
  (displayln (newtons-method (cubic 30 299 990) -9.5)) ; roots at -9,-10,-11
  (displayln (newtons-method (cubic 30 299 990) -10.5))
)

(newtons-method-test)

; Ex 1.41
; Doubling procedure

; Write a procedure (double) that applies a procedure twice


; Testing
(displayln "Testing double")
((double inc) 3) ; 5

; What is the expected result of this expression?
;(((double (double double)) inc) 5) 

; Ex 1.42
; Composition of functions

; Define (compose) so that it produces a procedure that applies f to g of x.


; Testing
(displayln "Testing compose")
((compose square inc) 6) ; 49

; Ex 1.43
; Repeated application of function

; Define (repeated) to be a function that gives n applications of f to x.

; Testing
(displayln "Testing repeated")
((repeated square 2) 5) ; 625
;((repeated square 0) 3) ; what happens? [invalid input by the problem statement]
  
; Ex 1.44
; Smoothing function

; Create a function that will smooth a function - giving the average of the values in the
; range (x +/- dx)


; Testing
(displayln "Testing smooth")
((smooth (lambda(x) (* x x x))) 1.0)  ; already smooth
((smooth abs) 0.0)  ; non-smooth
((smooth (lambda(x) (if (> x 0) 1 0))) 0.0) ; discontinuous (step function)
((smooth (lambda(x) (/ 1 x))) 0.0) ; can't be smoothed


; Show how to produced an n-fold smooth function

; Test e.g. on the step function:
;((<??> (lambda(x) (if (> x 0) 1.0 0.0))) 0.0)

; Ex 1.45
; Average damping for roots

; Determine how much average damping is necessary for the nth root using the fixed-point method.

; Write a procedure to find the nth-root of a polynomial

(displayln "Testing nth root function")
(<?nth-root?> 7 279936) ; 6
(<?nth-root?> 5 -243.0) ; -3
;(<?nth-root?> 0.5 7) ; what happens? [not valid input]


; Ex 1.46
; Iterative improvement

; Write a procedure (iterative-improve) that solves by successive iteration.


; Implement (sqrt) and (fixed-point) using this procedure

; Testing
(displayln "Testing (iterative-improve) using sqrt")
(sqrt 64)
(sqrt 2)
(sqrt 0)
; (sqrt -1) ; [invalid input]

; Testing
(displayln "Testing (iterative-improve) with fixed-point (Newton's method solver).")
(newtons-method-test)


; Section 1.3.3.


; Support procedures

(define (average x1 x2)
  (/ (+ x1 x2) 2.0)
  )

(define tolerance 0.00001) 

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)
    )
  (define (try guess) 
    (let ((next (f guess)))
      (if (close-enough? guess next) 
          next
          (try next)
          )
      )
    )
  (try first-guess)
  )

; Ex 1.35
; Defining phi using fixed-point

; Verifying
(displayln "Verifying value of phi using (fixed-point)")
(fixed-point <??> <?guess?>)


; Ex 1.36
; Checking steps taken in fixed point calculations

; Modify (fixed-point) to display the steps taken

; Testing
(displayln "Testing fixed-point displaying steps")
(fixed-point (lambda(x) (/ (log 1000) (log x))) 10.0)

(displayln "Testing fixed-point displaying steps with average damping")
(fixed-point (lambda(x) (average x (/ (log 1000) (log x)))) 10.0)

; Ex 1.37
; Continued fractions


; Test the continued fractions, and find how large k must be for accuracy to four decimal places. (1.6180)

; Testing
(displayln "Testing (cont-frac) by calculating phi")
(define k <??>)
(/ 1 
   (cont-frac (lambda (i) 1.0)
              (lambda (i) 1.0)
              k
              )
   )

; b. If (cont-frac) is recursive, make it iterative (or vice versa).

; Testing
(displayln "Testing (cont-frac) by calculating phi (iterative/recursive)")
(/ 1 
   (cont-frac (lambda (i) 1.0)
              (lambda (i) 1.0)
              k
              )
   )

; Ex 1.38.
; Continued Fraction for e

; Use the (cont-frac) procedure to estimate e using Euler's expansion.

; Testing
(displayln "Testing approximation to e using (cont-frac)")
; Expected value : 
(exp 1) ; e (if exp function is supported)

; Ex 1.39.
; Tangent using a continued fraction

; Use (cont-frac) to calculate the tangent function using Lambert's formula


; Testing 
(displayln "Testing tangent function using (cont-frac)")

; Sample values to test : 
; tan (/ pi 4.0) = 1.0
; tan (/ (* 2 pi) 5) = (sqrt (+ 5 (* 2 (sqrt 5))))





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

; Phi is expressed as phi = 1 + 1/phi, which can be written
; as phi^2 = phi + 1, or phi^2 - phi - 1 = 0.  The positive root
; of this equation is phi as defined in the text, or (1 + sqrt(5))/2
;
; The fixed point of the function (1 + 1/x) is thus a way to find phi.

; Verifying
(displayln "Verifying value of phi using (fixed-point)")
(fixed-point (lambda(x) (+ 1 (/ 1 x))) 1.0)


; Ex 1.36
; Checking steps taken in fixed point calculations

; Modify (fixed-point) to display the steps taken
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)
    )
  (define (show-and-go guess)
    (display guess)
    (newline)
    (try guess)
    )
  (define (try guess) 
    (let ((next (f guess)))
      (if (close-enough? guess next) 
          next
          (show-and-go next)
          )
      )
    )
  (try first-guess)
  )

; Testing
(displayln "Testing fixed-point displaying steps")
(fixed-point (lambda(x) (/ (log 1000) (log x))) 10.0)
; 32 steps
(displayln "Testing fixed-point displaying steps with average damping")
(fixed-point (lambda(x) (average x (/ (log 1000) (log x)))) 10.0)
; 10 steps using damping

; A few other values to try
;(fixed-point (lambda(x) (- (* x x) 2 )) 0.1)
;(fixed-point (lambda(x) (- (* x x) 2)) 2.1)
;(fixed-point (lambda(x) (average x (- (* x x) 1))) 1.0) ; try different guesses
;(fixed-point (lambda(x) (/ 1 (- x 1))) 1.61804)

; Ex 1.37
; Continued fractions
(define (cont-frac n d k)
  (define (iter partial k)
    (if (= 0 k)
        partial
        (iter (/ (n k) (+ (d k) partial)) (- k 1))
        )
    )
  (iter 0 k)
  )

; Test the continued fractions, and find how large k must be for accuracy to four decimal places. (1.6180)

; Testing
(displayln "Testing (cont-frac) by calculating phi")
(define k 12)
(/ 1 
   (cont-frac (lambda (i) 1.0)
              (lambda (i) 1.0)
              k
              )
   )

; 11 iterations (almost exactly) - this yields 1.6180555555555558

; b. If (cont-frac) is recursive, make it iterative (or vice versa).

(define (cont-frac n d k)
  (define (cont-frac-recursive i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-recursive (+ i 1))))
        )
    )
  (cont-frac-recursive 1)
  )

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

(define (eulery k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (x) (if (= 2 (remainder x 3))
                              (* 2 (+ 1 (quotient x 3)))
                              1
                              )
                    )
                  k)
     )
  )

; Testing
(displayln "Testing approximation to e using (cont-frac)")
(exp 1) ; e (if exp function is supported)
(eulery 100)

; Ex 1.39.
; Tangent using a continued fraction

; Use (cont-frac) to calculate the tangent function using Lambert's formula

(define (tan-cf x k) 
  (cont-frac (lambda (z)
               (if (= 1 z)
                   x
                   (* -1 (* x x))
                   )
               )
             (lambda (i)(- (* 2 i) 1))
             k
             )
  )

; Testing 
(displayln "Testing tangent function using (cont-frac)")
; Each pair should be equal (or nearly so)
1.0
(tan-cf (/ pi 4.0) 100) 

(sqrt (+ 5 (* 2 (sqrt 5))))
(tan-cf (/ (* 2 pi) 5) 100)            

;(tan-cf (/ pi 2.0) 20)


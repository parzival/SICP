; SECTION 1.1.7

(define square sqr) ; Squaring function

; Ex 1.6.
; Defining (if) using (cond)

; Because of the applicative-order processing of the interpreter, this leads to the else-clause always being executed. This creates an infinite recursion, since the check for termination is not a separate statement.  (It could be "fixed" by splitting the new-if statement, but that defeats the purpose of the redefinition).

(define (new-if predicate then-clause else-clause) 
  (cond (predicate then-clause)
        (else else-clause)
        )
  )

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

; Compare using (p) from previous sections.
(define (p) (p)
  )

(define (test1 x)
  (if (= x 0) x (p))
  )

(define (test2 x)
  (cond ((= x 0) x)
        (else (p))
        )
  )


(define (test3 x)
  (new-if (= x 0) x (p))
  )

(test1 0)
(test2 0)
;(test3 0)

; Ex 1.7.
; Deciding a sufficient answer for square root

(define (average x y)
  (/ (+ x y) 2)
  )

(define (improve guess x) 
  (average guess (/ x guess))
  )

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
  )

(define (sqrt-iter guess x) 
  (if (good-enough? guess x)
      guess (sqrt-iter (improve guess x)
                       x
                       )
      )
  )

(define (sqrt-old x) (sqrt-iter 1.0 x))

; define new functions
(define (sqrt-iter-new guess prev_guess x)
  (if (good-enough-new? guess prev_guess)
      guess 
      (sqrt-iter-new (improve guess x) guess x)
      )
  )

(define (good-enough-new? guess prev_guess)
  (< (abs (/ (- guess prev_guess) guess)) 0.001) 
  )

(define (sqrt-new x) (sqrt-iter-new 1.0 0 x))


; Testing square roots
; (sqrt) is the built-in procedure

; Small numbers 
(sqrt-old 3)
(sqrt-new 3)
(sqrt 3)
(newline)
(sqrt-old 0.00101)
(sqrt-new 0.00101)
(sqrt 0.00101)
(newline)
(sqrt-old 0.001)
(sqrt-new 0.001)
(sqrt 0.001)
(newline)
(sqrt-old 0.00099)
(sqrt-new 0.00099)
(sqrt 0.00099)
(newline)
(sqrt-old 0.000005)  
(sqrt-new 0.000005)
(sqrt 0.000005)
(newline)
; Large numbers
(sqrt-old 9e16)
(sqrt-new 9e16)   ; Compare this value to sqrt-old
(sqrt 9e16)
(newline)
;(sqrt-old 1e140) ; FAILS (doesn't stop running)
(sqrt-new 1e140)
(sqrt 1e140)
(newline)
; A few extra tests
(sqrt 0)
(sqrt 64e310)
(sqrt-old 0)      ; succeeds, but wrong
; (sqrt-new 0)      ; FAILS
; (sqrt-new 64e310) ; FAILS 
(newline)

; Ex 1.8. 
; Using Newton-Raphson to get the cube root

; Previously defined procedure from 1.7
; is usable for (good-enough?).

(define (cbrt-iter guess prev_guess x) 
  (if (good-enough-new? guess prev_guess)
      guess 
      (cbrt-iter (improve-cube guess x) guess x)
      )
  )

(define (improve-cube guess x) 
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)
  )

(define (cbrt x)
     (cbrt-iter 1.0 0.0 x)
  )

; Tests for cube root

; Most answers will not be exact
; since these are floating-point values
(cbrt 8)   ; 2
(cbrt 20.08) ; 2.7180320...
(cbrt 0) ; 0
(cbrt 1)   ; 1.0 
(cbrt 1.001) ; 1.00033322...  
(cbrt -216) ; -6
(cbrt 0.000008) ; 0.02
(cbrt 1e9) ; 1000
(cbrt 64e180) ; 4e60
;(cbrt 64e307) - hits the limit, FAILS.
;(cbrt 0.0)  ; FAILS.

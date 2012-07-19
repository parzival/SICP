; SECTION 1.1.7

(define square sqr) ; Squaring function

; Ex 1.6.
; Defining (if) using (cond)


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
; One should be called (sqrt-new)


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
;(sqrt-old 1e140) ; very likely fails to find an answer
(sqrt-new 1e140)
(sqrt 1e140)
(newline)

; Ex 1.8. 
; Using Newton-Raphson to get the cube root



; Tests for cube root

; Most answers will not be exact
; since these are floating-point values
(<?procedure-name?> 8)   ; 2
(<?procedure-name?> 20.08) ; 2.7180320...
(<?procedure-name?> 0) ; 0
(<?procedure-name?> 1)   ; 1.0 
(<?procedure-name?> 1.001) ; 1.00033322...  
(<?procedure-name?> -216) ; -6
(<?procedure-name?> 0.000008) ; 0.02
(<?procedure-name?> 1e9) ; 1000
(<?procedure-name?> 64e180) ; 4e60


; Section 1.2.4

(define (square x) (sqr x))

; Ex 1.16.
; Iterative exponentiation in log time

; Recursive 'fast' expt
(define (fast-expt b n) 
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2)))) 
        (else (* b (fast-expt b (- n 1))))
        )
  )

; Design an iterative exponentiation procedure using the successive squaring technique.

(define (expo b n)  <? call iterative proc with b and n ?> )

; Verifying with original
(displayln "Verifying fast-expt for exponentiation")
(fast-expt 2 9)       ; 512
(fast-expt 0.5 2)     ; 0.25
(fast-expt 3 0)       ; 1
(fast-expt 1 13)      ; 1
(fast-expt 45 13)     ; 3102863559971923828125
(fast-expt 367 2978)  ; ... a very large number

; Testing
(displayln "Testing iterative fast exponentiation")
(expo 2 9)       ; 512
(expo 0.5 2)     ; 0.25
(expo 3 0)       ; 1
(expo 1 13)      ; 1
(expo 45 13)     ; 3102863559971923828125
(expo 367 2978)  ; ... a very large number
(= (fast-expt 62756 107823) (expo 62756 107823)) ; should be true

; Ex 1.17
; Multiplication in log time

(define (double n)
  (+ n n)
  )

(define (halve n)
  (/ n 2)
  )

; Define a multiplication routine similar to fast-expt

; Testing 
(define big-result (* 641345464784622007 871320007945104960))
(newline)
(displayln "Testing fast multiplication")
(<? fast mult ?> 3 5)     ; 15 
(<? fast mult ?> 1 0)     ; 0
(<? fast mult ?> -6 13)   ; -78
(<? fast mult ?> 987654321 123456789) ; 121932631112635269
(= big-result (<? fast mult ?> 641345464784622007 871320007945104960))

; Ex 1.18.
; Iterative fast multiplication 

; Testing
(displayln "Testing fast multiplication (iterative)")
(<? iterative mult ?> 3 5)     ; 15 
(<? iterative mult ?> 1 0)     ; 0
(<? iterative mult ?> -6 13)   ; -78
(<? iterative mult ?> 987654321 123456789) ; 121932631112635269
(= big-result (<? iterative mult ?> 641345464784622007 871320007945104960))

; Ex 1.19
; Fast Fibonacci calculation

(define (fib n)
  (fib-iter 1 0 0 1 n)
  )

(define (fib-iter a b p q count)
  (cond 
    ((= count 0) b)
    ((even? count) (fib-iter  a
                              b
                              <??>	; compute p'
                              <??>	; compute q'
                             (/ count 2)
                             )
                   ) 
    (else (fib-iter (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- count 1)
                    )
          )
    )
  )

; Testing
(newline)
(displayln "Testing fast Fibonacci calculation") 
(fib 0)  ; 0  
(fib 1)  ; 1
(fib 5)  ; 5
(fib 7)  ; 13
(fib 83) ; 99194853094755497
(time (void (fib 4000000))) ; ... a very large number

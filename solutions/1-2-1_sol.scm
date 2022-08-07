; SECTION 1.2.1

; Ex 1.9. 
; Iterative and Recursive Substitution

; Racket(Pretty Big)-specific - use other forms, e.g. 1+ / -1+ instead of add/sub1 for MIT Scheme
(define (inc x) (add1 x)) 
(define (dec x) (sub1 x))

; Show the substitution process for these two implementations of + when evaluating (+ 4 5). 

; Procedure #1
(define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))
        )
  )

(+ 4 5)                          
(if (= 4 0) 5 (inc (+ (dec 4) 5)))   ; if is false for each of these steps
(inc (+ (dec 4) 5))                  ; (dec step condensed hereafter)
(inc (+ 3 5))
(inc (if (= 3 0) 5 (inc (+ (dec 3) 5))))
(inc (inc (+ 2 5)))
(inc (inc (if (= 2 0) 5 (inc (+ (dec 2) 5)))))
(inc (inc (inc (+ 1 5))))
(inc (inc (if (= 1 0) 5 (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))    
(inc (inc (inc (inc (if (= 0 0) 5 (+ (inc (dec 0) 5))))))) ; if evaluates to true
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

; Procedure #2
(newline)

(define (+ a b) 
    (if (= a 0)
        b
        (+ (dec a) (inc b))
        )
    )

(+ 4 5)
(if (= 4 0) 5 (+ (dec 4) (inc 5)))  ; if is false at each step
(+ (dec 4) (inc 5))                 ; (dec/inc step condensed hereafter)
(+ 3 6)
(if (= 3 0) 6 (+ (dec 3) (inc 6)))                 
(+ 2 7)
(if (= 2 0) 7 (+ (dec 2) (inc 7)))
(+ 1 8)
(if (= 1 0) 8 (+ (dec 1) (inc 8)))
(+ 0 9)                            
(if (= 0 0) 9 (+ (dec 0) (inc 9))) ; if evaluates to true
9

; The first is linear recursive.  The second is iterative (the procedure, but not the process, is recursive).

; Ex 1.10.
; Ackermann's function

(define (A x y) 
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y)) 
        ((= y 1) 2) 
        (else (A (- x 1)
                 (A x (- y 1))
                 )
              )
        )
  )

; Give the values returned by the following:
(A 1 10) ; 1024
(A 2 4)  ; 65536
(A 3 3)  ; 65536

; Give a mathematical description of what these functions
; do for positive integer n.
(define (k n) (* 5 n n)) ; Example: 5n^2

(define (f n) (A 0 n))   ; 2*n
(define (g n) (A 1 n))   ; 2^n
(define (h n) (A 2 n))   ; 2^2^...[n times]...^2

; Examples for verification
(k 4) ; 80
(f 4) ; 8
(g 4) ; 16
(h 4) ; 2^2^2^2 = 2^16 = 65536

(k 5) ; 125
(f 5) ; 10
(g 5) ; 32
;(h 5) ; 2^2^2^2^2, or 2^65536, a rather large number  

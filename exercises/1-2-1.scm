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

; Procedure #2
(newline)

(define (+ a b) 
    (if (= a 0)
        b
        (+ (dec a) (inc b))
        )
    )

(+ 4 5)


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
(A 1 10) 
(A 2 4)  
(A 3 3)  

; Give a mathematical description of what these functions
; do for positive integer n.
(define (k n) (* 5 n n)) ; Example: 5n^2

(define (f n) (A 0 n))   
(define (g n) (A 1 n))   
(define (h n) (A 2 n))  
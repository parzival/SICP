; SECTION 1.2.2

; Ex 1.11.
; Defining a function (recursively & iteratively)

; Recursive method
(define (fun n)
  (cond ((< n 3) n)
        (else (+ (fun (- n 1)) (fun (- n 2)) (fun (- n 3)) ))
        )
  )

; Testing fun
(fun 0)
(fun 1)
(fun 4)
(fun 5)
(fun 10)
(fun 20)
;(fun 40)  ; takes too long
;(fun 100) ; takes too long
(fun 5.6)  ; n is implied to be an integer, but this still works.
(fun -1)   ; n is also implied to be postive.

; Iterative method
(define (fun-iter x y z index)
  (cond ((<= index 0 ) z)
        (else (fun-iter (+ x y z) x y (- index 1)))
        )
  )

(define (fun n)
  (fun-iter 2 1 0 n)
  )

; Testing fun
(fun 0)
(fun 1)
(fun 4)
(fun 5)
(fun 10)
(fun 20)
(fun 40) 
(fun 100) 
;(fun 10000) ; big number, but still quite fast
(fun 5.6) ; wrong answer for non-integer n.
(fun -1)  ; wrong answer for negative n.

; Ex 1.12.  
; Express an element of Pascal's triangle recursively.

(define (pascal row indx)
  (cond ((or (> indx row) (< indx 1)) 0)
        ((= 1 row) 1) 
        (else (+ (pascal (- row 1) (- indx 1)) (pascal (- row 1) indx)))
        )
  )

; Testing
(pascal 0 0)    ; 0
(pascal 1 1)    ; 1
(pascal 4 4)    ; 1
(pascal 5 1)    ; 1
(pascal 5 2)    ; 4
(pascal 5 3)    ; 6
(pascal 5 4)    ; 4
(pascal 5 5)    ; 1
(pascal 10 6)   ; 126
(pascal 20 7)   ; 27132
(pascal 40 2)   ; 39
(pascal 5000 1) ; 1
;(pascal 40 21) ; takes too long
(pascal 4 6)    ; 0 (invalid input)

; Ex 1.13.
; Prove that Fib(n) is the closest integer to (phi^n)/sqrt(5), where phi = (1 + sqrt(5))/2.
;
;First off:  
;phi^2 = phi + 1  - by definition of phi
;phi^3 = phi^2*phi = phi^2 + phi  - substitution of phi
;
;It follows from this that, for n>1:
;
;phi^n = phi^n-1 + phi^n-2 
;
;Similarly, psi^n = psi^n-1 + psi^n-2, where psi = (1-sqrt(5))/2
;
;Assume Fib(k) = (phi^k-psi^k)/sqrt(5) and Fib(k-1) = (phi^(k-1)-psi^(k-1))/sqrt(5)
;
;Fib(k+1) = Fib(k) + Fib(k-1)
;
;         = [(phi^k - psi^k) + (phi^k-1 - psi^k-1)]/sqrt(5)
;         = (phi^k+1 - psi^k+1)/sqrt(5)                      / Based on the above result
;         = (phi^k+1)/sqrt(5) - psi^k+1/sqrt(5)
;         
;For k = 0
;Fib(0) ?= (phi^0 - psi^0)/sqrt(5)
;0 ?= (1 - 1)/sqrt(5)
;0 = 0
;
;For k = 1
;Fib(1) ?= (phi^1 - psi^1)/sqrt(5)
;1 ?= [1 + sqrt(5) - (1 - sqrt(5))]/(2*sqrt(5))
;1 ?= [ 2 * sqrt(5)]/(2 * sqrt(5))
;1 = 1
;         
;Thus, by induction, Fib(n) = (phi^n- psi^n)/sqrt(5).
;         
;|psi^n|/sqrt(5) < 1/2 for any n>=0  - since |psi| < 1
;      
;Therefore (phi^n)/sqrt(5) - psi^n/sqrt(5) never varies more than 1/2 from Fib(n). QED.


; SECTION 1.2.2

; Ex 1.11.
; Defining a function (recursively & iteratively)

; Recursive method


; Testing fun
(fun 0)
(fun 1)
(fun 4)
(fun 5)
(fun 10)
(fun 20)
;(fun 40)  ; takes too long
;(fun 100) ; takes too long

; Iterative method


; Testing fun
(fun 0)
(fun 1)
(fun 4)
(fun 5)
(fun 10)
(fun 20)
(fun 40) 
(fun 100) 

; Ex 1.12.  
; Express an element of Pascal's triangle recursively.


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
(pascal 4 6)    ; (invalid input)

; Ex 1.13.
; Prove that Fib(n) is the closest integer to (phi^n)/sqrt(5), where phi = (1 + sqrt(5))/2.
; (Hint: Prove that Fib(n) = (phi^n - psi^n)/(sqrt 5), where psi = (1 - sqrt(5))/2

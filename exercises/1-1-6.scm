; Section 1.1.6

; Ex 1.1. 
; See what the interpreter responds for this sequence.
;
; (It is best to enter these commands separately
; on your own, to see what happens at each step.)
10 
(+ 5 3 4) 
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3) 
(define b (+ a 1))
(+ a b (* a b)) 
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a) 
         ((< a b) b)
         (else -1))
   (+ a 1))

; Ex 1.2.
; Put the expression in prefix form.

; Ex 1.3. 
; Define a procedure that sums the squares of the two larger of three.

; Testing
;(<?procedure-name?> 1 2 3)        ; 13
;(<?procedure-name?> 5 6 3)        ; 61
;(<?procedure-name?> -2 -4 -1)     ; 5
;(<?procedure-name?> 0.8 0.2 0.5)  ; 0.89
;(<?procedure-name?> 0.5 -1 0)     ; 0.25
;(<?procedure-name?> 2 2 3)        ; 13
;(<?procedure-name?> 3 2 2)        ; 13
;(<?procedure-name?> 1 1 1)        ; 2

; Ex 1.4. 
; Describe the behavior of this procedure.
(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))


; Ex 1.5. 
; Describe result using applicative vs. normal order.

(define (p) (p))
(define (test x y) (if (= x 0)
                       0
                       y))

;Then he evaluates the expression
;(test 0 (p))

; Applicative-order:



; Normal order:


; Explain why:

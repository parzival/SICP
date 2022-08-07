; Section 1.1.6  - SOLUTIONS

; Ex 1.1. 
; See what the interpreter responds for this sequence.
10 
; 10
(+ 5 3 4)
; 12 
(- 9 1)
; 8  
(/ 6 2)
; 3
(+ (* 2 4) (- 4 6))
; 6
(define a 3) 
; ;nothing
(define b (+ a 1))
; ;nothing
(+ a b (* a b)) 
; 19
(= a b)
; #f 
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a) b a))
; 6
(* (cond ((> a b) a) 
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16

; Ex 1.2.
; Put the expression in prefix form.
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
; = -74/300 

; Ex 1.3. 
; Define a procedure that sums the squares of the two larger of three.

(define (bigger-sum-of-squares  a b c)
                              (cond
                              ((and (<= a b) (<= a c)) (+ (* b b) (* c c)))
                              ((and (<= b a) (<= b c)) (+ (* a a) (* c c)))
                              (else              (+ (* a a) (* b b)))
                              )
)

;Testing

(bigger-sum-of-squares 1 2 3)        ; 13
(bigger-sum-of-squares 5 6 3)        ; 61
(bigger-sum-of-squares -2 -4 -1)     ; 5
(bigger-sum-of-squares 0.8 0.2 0.5)  ; 0.89
(bigger-sum-of-squares 0.5 -1 0)     ; 0.25
(bigger-sum-of-squares 2 2 3)        ; 13
(bigger-sum-of-squares 3 2 2)        ; 13
(bigger-sum-of-squares 1 1 1)        ; 2

; Ex 1.4. 
; Describe the behavior of this procedure.
(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))

; The if expression evaluates to an operator, either + or - depending on what b is.
; When b > 0, the procedure returns a+b.  If b <= 0 , returns a-b.  Note that in both cases, the result is a + |b|.
(a-plus-abs-b 1 2)  ; 3
(a-plus-abs-b 1 -2) ; 3
(a-plus-abs-b -1 0) ; -1

; Ex 1.5. 
; Describe result using applicative vs. normal order.

(define (p) (p))
(define (test x y) (if (= x 0)
                       0
                       y))

;Then he evaluates the expression
;(test 0 (p))

; Applicative-order:
;
; (test 0 (p))       ; substitute the body of the procedure =>
; (if (= x 0) 0 y)   ; and evaluate each argument 
;; x=>0 y=>(p)         
;;  0, (p)=>(p)       ; keep evaluating to a primitive  
;;  0, (p)=>(p)       ; and again 
;;  0, (p)=>(p)       ; and again 
; ... ad infinitum

; Normal order:
;
; (test 0 (p))       ; substitute the body of the procedure =>
; (if (= x 0) 0 y)   ; with x = 0, y = (p) =>
; (if (= 0 0) 0 (p)) ; evaluate arguments as needed =>
; (if true 0 (p))    ; predicate is true, evaluate first branch =>
; 0                  ; done.
; since 0=0, the result is 0.

;  Explain why:
;  (p) is a recursive function without termination.  It never resolves to a primitive, and any
;  evaluation of it leads to an infinite number of calls.  In applicative-order evaluation, it 
;  will be evaluated even when not needed, and thus the procedure does not return.  In normal order,
;  the problematic (p) is not evaluated in this call, so the procedure terminates.
; Section 2.1.3


; Ex 2.4
; Procedural representation of pairs

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))

; Verify that the definitions above work properly 
; This should be x for any x and y
; (car (cons x y))
(displayln "Verifying procedural (cons) and (cdr)")
(car (cons true false)) ; true
(car (cons 1 2)) ; 1
(car (cons "a" "b")) ; "a"
(car (cons + - )) ; procedure:+
(car (cons (cons 0 1) (cons "a" "b"))) ; procedure (defined cons above), as (cons 0 1)

; Define cdr using this representation
(define (cdr z) (z (lambda (p q) q)))

; Testing
(displayln "Testing procedural cdr")
(cdr (cons 3 4)) ; 4
(cdr (cons + - )) ; procedure:-
(car (cdr (cons 3 (cons 5 6)))) ; 5

; Substitution example:
; (cdr (cons 3 4))

; Insert argument into body of cdr
; ((cons 3 4) (lambda (p q) q))

; Evaluate argument (cons 3 4) by replacing with the body of cons,
; with 3 and 4 for x and y.
;  ((lambda(m) (m 3 4)) (lambda (p q) q)) 

; Evaluate (lambda(m)(m 3 4) by inserting argument into body of lambda expr.
; ((lambda (p q) q) 3 4)

; Evaluate (lambda (p q) q) by inserting arguments into body of lambda expr.
; 4

; Ex 2.5
; Representing nonnegative integers with calculations

; Since 2 and 3 are relatively prime, any representation of a number by 2^a*3^b can be recovered by decomposition into prime factors.  With a and b nonnegative, each single value can only be decomposed one way.  It will have (a factors = 2) and (b factors = 3). 

(define (cons-int a b)
  (* (expt 2 a) (expt 3 b))
  )

(define (car-int p)
  (define (car-iter p i)
    (if (even? p)
      (car-iter (/ p 2) (+ 1 i))
      i
      )
    )
  (car-iter p 0)
  )

(define (cdr-int p)
  (define (cdr-iter p i)
    (if (= 0 (remainder p 3))
        (cdr-iter (/ p 3) (+ 1 i))
        i
        )
    )
  (cdr-iter p 0)
  )

; Testing
(newline)
(displayln "Testing pairs representing integers with a single value")
(cons-int 5 1)  
(cdr-int (cons-int 3 4))
(car-int (cons-int 0 1))
;(cdr-int (cons-int -1 -2)) ; invalid input
 
; Ex 2.6
; Church numerals

(define zero (lambda (f) (lambda (x) x)))
; equal to 
; (define (zero f) (lambda(x) x))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
  )

; Define one and two directly 
(define one (lambda(f) (lambda(x) (f x))))
(define two (lambda(f) (lambda(x) (f (f x)))))

(define (church+ a b)
  (lambda(f) (lambda(x) ((a f) ((b f) x))))
  )

(define three (church+ one two))

; Testing
(newline)
(displayln "Testing Church numerals")
(define (add-s-to-str str)
  (string-append "S" str)
  )

((one add-s-to-str) "0")
((two add-s-to-str) "0")
((three add-s-to-str) "0")
(((church+ two two) add-s-to-str) "0")
; Should yield identical results (note use of equal?)
; All should return true.
(equal? (((add-1 zero) add-s-to-str) "0") ((one add-s-to-str) "0"))
(equal? (((add-1 one) add-s-to-str) "0") ((two add-s-to-str) "0"))
(equal? (((add-1 two) add-s-to-str) "0") ((three add-s-to-str) "0"))
(equal? (((church+ one two) add-s-to-str) "0") ((three add-s-to-str) "0"))

; Note that this is false:
(equal? two (add-1 one))

; Test some other procedures
(define (decr x)
  (- x 1)
  )

(define (double x) 
  (* 2 x)
  )

((three decr) 0)

((two double) 5)

(= (((add-1 (add-1 (add-1 one))) double) 2) (((add-1 three) double) 2))
(= (((add-1 (add-1 three)) decr) 0) (((church+ two three) decr) 0))
  


  


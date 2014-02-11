; Section 2.1.3


; Ex 2.4
; Procedural representation of pairs

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))

; Verify that the definitions above work properly 
; This should be x for any x and y
; (car (cons x y))
(displayln "Verifying procedural (cons) and (cdr)")

; Define cdr using this representation

; Testing
(displayln "Testing procedural cdr")
(cdr (cons 3 4)) ; 4
(cdr (cons + - )) ; procedure:-
(car (cdr (cons 3 (cons 5 6)))) ; 5

; Ex 2.5
; Representing nonnegative integers with calculations


; Testing
(newline)
(displayln "Testing pairs representing integers with a single value")
(<?cons?> 5 1) ; stored value = 96
(<?cdr?> (<?cons?> 3 4))  ; 4
(<?car?> (<?cons?> 0 1))  ; 0
;(<?cdr?> (<?cons?> -1 -2)) ; invalid input
 
; Ex 2.6
; Church numerals

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
  )

; Define one and two directly 

; Define addition
(define <?+?>)

(define three (<?+?> <?one?> <?two?>))

; Testing
(newline)
(displayln "Testing Church numerals")
(define (add-s-to-str str)
  (string-append "S" str)
  )

((<?one?> add-s-to-str) "0")
((<?two?> add-s-to-str) "0")
((three add-s-to-str) "0")
(((<?+?> two two) add-s-to-str) "0")

; Should yield identical results (note use of equal?)
; All should return true.
(equal? (((add-1 zero) add-s-to-str) "0") ((<?one?> add-s-to-str) "0"))
(equal? (((add-1 <?one?>) add-s-to-str) "0") ((<?two?> add-s-to-str) "0"))
(equal? (((add-1 <?two?>) add-s-to-str) "0") ((three add-s-to-str) "0"))
(equal? (((<?+?> one two) add-s-to-str) "0") ((three add-s-to-str) "0"))
; Test another procedures
(define (decr x)
  (- x 1)
  )

; yields negative integers
((three decr) 0)
; These should be equal
(= (((add-1 (add-1 three)) decr) 0) (((<?+?> <?two?> three) decr) 0))

  


  


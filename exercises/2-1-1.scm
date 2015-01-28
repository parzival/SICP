; Section 2.1.1

; Supplied rational arithmetic functions

(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))
            )
  )

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer  y) (denom x)))
            (* (denom x)  (denom y))
            )
  )

(define (mul-rat x y) 
  (make-rat (* (numer x) (numer y))
            (* (denom x)  (denom y))
            )
  )

(define (div-rat x y) 
  (make-rat (* (numer x) (denom y))
            (* (denom x)  (numer y))
            )
  )

(define (equal-rat? x y) 
  (= (* (numer x) (denom y))
     (* (numer y) (denom  x))
     )
   )

(define (make-rat n d) (cons n d)) 

(define (numer x) (car x)) 

(define (denom x) (cdr x))

(define (print-rat x) 
  (newline)
  (display (numer x)) 
  (display "/")
  (display (denom x))
  )

(display "Verifying positive values")
(define one-half (make-rat 1 2)) 
(print-rat one-half) ; 1/2
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third)) ; 5/6
(print-rat (mul-rat one-half one-third)) ; 1/6
(newline)
(display "1/2 + 1/3 = ")
(print-rat (add-rat one-third one-third)) ; 6/9
; A few more (not from the book)
(print-rat (add-rat (make-rat 2 5) (make-rat 4 5)))  ; 6/5
(print-rat (sub-rat (make-rat 6 8) (make-rat 7 15))) ; 17/60 
(print-rat (mul-rat (make-rat 1 2) (make-rat 3 7)))  ; 3/14
(print-rat (div-rat (make-rat 3 10) (make-rat 9 5))) ; 1/6 
(define a1 (random 100000))
(define a2 (random 100000))
(print-rat (make-rat (min a1 a2) (max a1 a2)))
(print-rat (make-rat 0 100))
(print-rat (add-rat (make-rat 0 100) (make-rat 0 5)))
(newline)

(define (make-rat n d) 
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))
    )
  )

(newline)
(display "Verifying positive values (reduced terms)")
(newline)
(display "1/2 + 1/3 = ")
(print-rat (add-rat one-third one-third)) ; 2/3 (no need to redefine the underlying values)
(print-rat (add-rat (make-rat 2 5) (make-rat 4 5)))  
(print-rat (sub-rat (make-rat 6 8) (make-rat 7 15))) 
(print-rat (mul-rat (make-rat 1 2) (make-rat 3 7)))  
(print-rat (div-rat (make-rat 3 10) (make-rat 9 5)))
(print-rat (make-rat (min a1 a2) (max a1 a2)))
(print-rat (make-rat 0 100))
(print-rat (add-rat (make-rat 0 100) (make-rat 0 5)))
(newline)

(newline)
(display "Testing negative values (prior to changes)")
(print-rat (make-rat -1 3))  ; -1/3
(print-rat (make-rat 1 -3))  ; -1/3
(print-rat (make-rat -4 -5)) ;  4/5
(print-rat (make-rat 0 -1))  
(newline)


; Ex 2.1
; Improving make-rat to allow negative numbers


; Testing 
; Make sure positive values work okay
(newline)
(display "Testing positive values")
(newline)
(display "1/2 + 1/3 = ")
(print-rat (add-rat one-third one-third)) ; 2/3 (no need to redefine the underlying values)
(print-rat (add-rat (make-rat 2 5) (make-rat 4 5)))  
(print-rat (sub-rat (make-rat 6 8) (make-rat 7 15))) 
(print-rat (mul-rat (make-rat 1 2) (make-rat 3 7)))  
(print-rat (div-rat (make-rat 3 10) (make-rat 9 5)))
(print-rat (make-rat (min a1 a2) (max a1 a2)))
(print-rat (make-rat 0 100))
(print-rat (add-rat (make-rat 0 100) (make-rat 0 5)))
(newline)


(newline)
(display "Testing negative values")
; Basic creation
(print-rat (make-rat -1 3))  ; -1/3
(print-rat (make-rat 1 -3))  ; -1/3
(print-rat (make-rat -4 -5)) ;  4/5
(print-rat (make-rat 0 -1)) 
; Arithmetic operations
(print-rat (add-rat (make-rat -2 5) (make-rat -4 5)))  ; -6/5
(print-rat (sub-rat (make-rat -6 8) (make-rat -7 15))) ; -17/60
(print-rat (mul-rat (make-rat -1 -3) (make-rat 1 -3))) ; -1/9
(print-rat (mul-rat (make-rat -1 3) (make-rat -1 3)))  ;  1/9
(print-rat (div-rat (make-rat 7 16) (make-rat -3 14))) ; -49/24
; Test against zero
(print-rat (mul-rat (make-rat 0 -1) (make-rat 2 1))) ; 0/1 
; Test combining with positives
(print-rat (add-rat (make-rat 1 5) (make-rat 4 -5))) ; -3/5
; Test as a result of computation
(print-rat (sub-rat (make-rat 1 5) (make-rat 4 5)))  ; -3/5

 



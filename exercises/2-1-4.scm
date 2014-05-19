; Section 2.1.4

(define (add-interval x y) 
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
                 )
  )

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y))) 
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
    )
  )

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y)) 
                               (/ 1.0 (lower-bound y))
                               )
                )
  )


; Ex 2.7
; Interval arithmetic

(define (make-interval a b) (cons a b))

; Define (upper-bound) and (lower-bound)

; Helper for testing
(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
  )

; Tests
(displayln "Testing interval arithmetic")
(define R1 (make-interval 4 5))
(define R2 (make-interval 2 3))
(define R3 (make-interval -2 2))

(print-interval (add-interval R2 R3)) ; [0,-5]
(newline)
(print-interval (div-interval R1 R2)) ; [1 1/3, 2 1/2]
(newline)
(print-interval (mul-interval R3 R1)) ; [-10,10]
(newline)


; Ex 2.8 
; Define (sub-interval) to perform subtraction

; Tests
(newline)
(display "Testing interval subtraction")
(newline)
(print-interval (sub-interval R1 R2))
(newline)
(print-interval (sub-interval R3 R1))
(newline)
(print-interval (add-interval R2 (sub-interval R1 R2)))  ; R2 + R1 - R2 =? 


; Ex 2.9
; Width of an interval in arithmetic


; Ex 2.10
; Division by 'zero' check

; Checks if two intervals are equal
(define (eql-interval a b)
  (and (= (lower-bound a) (lower-bound b)) (= (upper-bound a) (upper-bound b)))
  )

; Tests
(define R4 (make-interval -1 3))

(newline)
(display "Testing division with zero-crossing detection.")
(newline)

(print-interval (div-interval R2 R1)) ; Safe; does not cross zero
(newline)
(print-interval (div-interval R2 R3)) ; Crosses zero, error expected
(print-interval (div-interval R2 R4)) ; Crosses zero, error expected
(print-interval (div-interval R3 R1)) ; Safe, answer crosses zero
(newline)


; Ex 2.11
; Breaking down multiplication

; Tests

(newline)
(displayln "Testing improved interval multiplication ")

; Labels with signs here refer to values in the expression 
; (x * y), x =[A,B], y=[C,D]  
;  Thus C- means that C is negative; A+ means that A is positive.
; Any value here could be replaced by one that completes the interval and has the proper sign (or is zero).

(define A+B+ (make-interval 1 2))
(define C+D+ (make-interval 3 5))
(define A-B+ (make-interval -7 11))
(define C-D+ (make-interval -13 17))
(define A-B- (make-interval -23 -19))
(define C-D- (make-interval -31 -29))
(define A0B+ (make-interval 0 1))
(define A0B0 (make-interval 0 0))
(define A-B0 (make-interval -1 0))
(define C0D+ (make-interval 0 2))
(define C-D0 (make-interval -2 0))

; Special test function
(define (test-report test-fn on-fail)
  (if (test-fn)
      (display "passed.")
      (on-fail)
      )
  (newline)
  )

(define (cmp-test x y)
  (define (report-failure r1 r2)
    (display "Failed multiplying ")
    (print-interval x) 
    (display " and ")
    (print-interval y)
    (display ": ")
    (print-interval r1)
    (display " expected but got ")
    (print-interval r2)
    )
  (let ((r1 (<?old multiplication?> x y))
        (r2 (<?new multiplication?> x y))
        )
    (test-report (lambda() (eql-interval r1 r2)) (lambda() (report-failure r1 r2)))
    )
  )


(displayln "Testing all cases with nonzero values.")
(cmp-test A+B+ C+D+)
(cmp-test A+B+ C-D+)
(cmp-test A+B+ C-D-)
(cmp-test A-B+ C+D+)
(cmp-test A-B+ C-D+)
(cmp-test A-B+ C-D-)
(cmp-test A-B- C+D+)
(cmp-test A-B- C-D+)
(cmp-test A-B- C-D-)
(displayln "Testing with zero values for x.")
(cmp-test A0B+ C+D+)
(cmp-test A0B+ C-D+)
(cmp-test A0B+ C-D-)
(cmp-test A0B0 C+D+)
(cmp-test A0B0 C-D+)
(cmp-test A0B0 C-D-)
(cmp-test A-B0 C+D+)
(cmp-test A-B0 C-D+)
(cmp-test A-B0 C-D-)
(displayln "Testing with zero values for y.")
(cmp-test A+B+ C0D+)
(cmp-test A+B+ A0B0)
(cmp-test A+B+ C-D0)
(cmp-test A-B+ C0D+)
(cmp-test A-B+ A0B0)
(cmp-test A-B+ C-D0)
(cmp-test A-B- C0D+)
(cmp-test A-B- A0B0)
(cmp-test A-B- C-D0)
(displayln "Testing with mixed zero values, and identical intervals.")
(cmp-test A0B+ C0D+)
(cmp-test A0B+ A0B0)
(cmp-test A0B+ C-D0)
(cmp-test A0B0 C0D+)
(cmp-test A0B0 A0B0)
(cmp-test A0B0 C-D0)
(cmp-test A-B0 C0D+)
(cmp-test A-B0 A0B0)
(cmp-test A-B0 C-D0)
(cmp-test A+B+ A+B+)
(cmp-test A-B0 A-B0)
(cmp-test A-B+ A-B+)
(cmp-test A-B- A-B-)

; Tolerances: additional functions
(define (make-center-width c w) (make-interval (- c w) (+ c w)))

(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))


; Ex 2.12
; Intervals defined in terms of tolerance

; Define (make-center-percent) and (percent)

; Tests

(newline)
(displayln "Testing percent tolerances.")
(print-interval (make-center-percent 5 20)) ; [4,6]

(newline) 
(percent (make-center-percent 1 10)) ; 10%
(percent (make-interval 90 110)) ; 10%
(= (width (make-interval 90 110)) (percent (make-interval 90 110)))
(percent (make-interval 7.6 8.4))  ; 5%
(percent (make-center-width 3.5 0.15))  ; ~4.3%
(percent (make-interval 5 5)) ; 0%
(percent (make-interval -6 -4)) ; 20%
(percent (make-interval -2 0))  ; 100%
(percent (make-interval -10 5)) ; 300%

; ; What is the result?
(define zer-per (make-center-percent 0 25))  
(percent zer-per)

(percent (make-interval -1 1))  


; Ex 2.13
; Approximation of tolerance

; If the interval is written as 
; (x +/- ax), (y +/- by)
; with x,y,a,b all positive values and a and b small values,
; the approximate tolerance of the answer can be written as 
; d = a+b, with the answer =~ (xy +/- d)

(displayln "Showing examples for tolerance approximation")

(define (show-tols xc xtol yc ytol)
  (display "xtol: ")
  (display xtol)
  (display " ytol: ")
  (display ytol)
  (newline)
  (let ((approx (+ xtol ytol))
        (calc  (percent (mul-interval (make-center-percent xc xtol)
                                      (make-center-percent yc ytol)
                                      )
                        )
               )
        )
    (display " Approximation: ")
    (display approx)
    (display " Calculated: ")
    (display calc)
    (display " Difference: ")
    (display (- approx calc))
    (newline)
    )
  )

(define x (+ 100 (random 10000)))
(define y (+ 100 (random 10000)))
(show-tols x 0.1 y 0.1)
(show-tols x 0.5 y 0.5)
(show-tols x 1 y 1)
(show-tols x 10 y 2)
(show-tols x 5 y 4)
(show-tols x 6 y 6)
(show-tols x 10 y 10)


; Ex 2.14
; Order of calculation

; Lem's procedures
(define (par1 r1 r2) 
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)
                )
  ) 

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1) (div-interval one r2))
                  )
    )
  )

(define (print-interval-percent i)
  (display "(")
  (display (center i))
  (display " +/- ")
  (display (percent i))
  (display "%)")
  )

; Demonstrating Lem's functions have differences


; Compute A/A vs. A/B 


; Ex 2.15
; Is par2 "better" for computing parallel resistances?


; Ex 2.16
; Explain why equivalent algebraic expressions lead to different answers.


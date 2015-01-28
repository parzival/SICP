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
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

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
; The difference of two intervals  a and b will have a lower bound equal to the lowest number that can be reached by subtracting from a, namely (lb(a) - ub(b)).  The upper bound is the largest difference computable, which is (ub(a) - lb(b)).
; Another way of looking at this : 
; Subtraction can be defined a + -b, so we only need to define negation. 
; A sensible definition of -x is (-ub(x), -lb(x)).  This is the approach taken here.

(define (neg-interval x) 
  (make-interval (- (upper-bound x)) (- (lower-bound x)))
  )

(define (sub-interval x y)
  (add-interval x (neg-interval y))
  )

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
; Width of an interval in arithmetic  (using width as 2*width as defined in book for easier writing)

; For a sum x +y, with x = [a,b] and y = [c,d]   a <= b, c <= d 
; Widths are : w(x) = b-a  w(y) = d-c
; x + y = [a+c, b+d]
; The width of this interval is 
; w(x+y) = (b+d) - (a+c)
;        = b-a + d-c
;        = w(x) + w(y)

; For subtraction, it is sufficient to show that w(-x) = w(x):
;    -x = [-b,-a]
;    w(-x) = -a + b
;          = b - a
;          = w(x)

; This is not the case for multiplication or division:

; Counterexamples
;       [3,5] * [-1,1] = [-5,5]
;Widths:  2        2        10    not 4

;       [3,6] / [1,2]  = [1.5,6]
;Widths:  3       1         4.5   not 3

; Ex 2.10
; Division by 'zero' check

(define (div-interval x y)
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
      (error "Division by zero-crossing interval not allowed -- DIV-INTERVAL" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y)) 
                                   (/ 1.0 (lower-bound y))
                                   )
                    )
      )
  )

; This was bothering me ...
(define (make-interval a b)
  (if (> a b)
      (error "Cannot make interval with upper bound less than lower bound -- MAKE-INTERVAL")
      (cons a b)
      )
  )

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
;(print-interval (div-interval R2 R3)) ; Crosses zero, error
;(print-interval (div-interval R2 R4)) ; Crosses zero, error
(print-interval (div-interval R3 R1)) ; Safe, answer crosses zero
(newline)


; Ex 2.11
; Breaking down multiplication

(define (mul-interval-imp x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y))
        )     
    (let ((ub (cond 
                ((and (>= b 0) (>= d 0)) (if (and (< a 0) (< c 0))
                                             (max (* b d) (* a c))
                                             (* b d)
                                             )
                                         )
                ((>= a 0) (* a d))
                ((and (< b 0) (>= c 0) (>= d 0)) (* b c))
                (else (* a c))
                )
              )
          (lb (cond
                ((>= a 0) (if (< c 0) 
                              (* b c) 
                              (* a c)
                              )
                          )
                ((>= d 0) (if (and (< c 0) (>= b 0))
                              (min (* a d) (* b c))
                              (* a d)
                              )
                          )
                ((< b 0)  (* b d))
                (else     (* b c))
                )
              )
          )
      (make-interval lb ub)
      )
    )
  )
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
  (let ((r1 (mul-interval x y))
        (r2 (mul-interval-imp x y))
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
(define (make-center-percent c p)
  (if (= c 0)
      (error "MAKE-CENTER-PERCENT -- Can't center interval on 0.")
      (make-center-width c (* (/ p 100) c))
      )
  )

(define (percent i)
  (if (= (center i) 0)
      (error "PERCENT -- Can't compute percent - interval centered at 0.")
      (* 100.0 (/ (width i) (abs (center i))))
      )
  )

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
;(define zer-per (make-center-percent 0 25))  ; Error in MAKE-CENTER-PERCENT (can't center on 0)
;(percent zer-per)

;(percent (make-interval -1 1))    ; Error in PERCENT (interval centered on 0) 


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

(define x (random 10000))
(define y (random 10000))
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
(newline)
(display "Showing parallel resistor function differences.")
(newline)
(define R1 (make-center-percent 1000 5))
(define R2 (make-center-percent 3000 5))

(displayln "  Method 1                                  Method 2")
(print-interval-percent (par1 R1 R2))
(print-interval-percent (par2 R1 R2))
(newline)

(define R3 (make-center-percent 3000 10))
(print-interval-percent (par1 R1 R3))
(print-interval-percent (par2 R1 R3))
(newline)

(define R4 (make-center-percent 1000 10))
(print-interval-percent (par1 R4 R3))
(print-interval-percent (par2 R4 R3))
(newline)
(print-interval-percent (par1 R4 R2))
(print-interval-percent (par2 R4 R2))
(newline)

; Computing A/A vs. A/B 
(newline)
(displayln "Showing A/A vs. A/B.")

(define A (make-center-percent 100 0.2))
(define B (make-center-percent 500 0.5))

(display "A: ")
(print-interval-percent A)
(newline)
(display "B: ")
(print-interval-percent B)
(newline)
(display "A/A: ")
(print-interval-percent (div-interval A A))
(display " B/B: ")
(print-interval-percent (div-interval B B))
(newline)
(define mul-id (make-center-percent 1 0))
; Comparing A/B to 1/(B/A)
(display "A/B: ")
(print-interval-percent (div-interval A B))
(display " 1/(B/A): ")
(print-interval-percent (div-interval mul-id (div-interval B A)))

(newline)
(display "(A*A)/A: ")
(print-interval-percent (div-interval (mul-interval A A) A))
(newline)

; Ex 2.15
; Is par2 "better" for computing parallel resistances?

; As an example, consider a circuit with two 1k, 5% resistors.  The maximum current will flow when the two resistors have their minimum value.  This is 950 ohms for each, which is equivalent to a single 475-ohm resistor.  Going the other way, the minimum current occurs when both are at their maximum value, 1050 ohms.  These two in parallel result in an equivalent resistance of 525 ohms.  The correct answer for equivalent resistance is [475, 525]

(newline)
(displayln "Comparing par1 and par2 for correctness.")
(displayln "Correct answer: [475,525]")
(display "Method 1:")
(print-interval (par1 R1 R1))
(newline)
(display "Method 2:")
(print-interval (par2 R1 R1))
(newline)

; Ex 2.16
; Explain why equivalent algebraic expressions lead to different answers.

; Speaking of "equivalent" algebraic expressions is incorrect, because the distributive property does not hold for this arithmetic. Also, multiplicative (and additive) inverses do not in general exist.  This is why typical algebra cannot be applied.

(newline)
(displayln "Demonstrating distributive property for intervals.")

; Modify the test function from before to make a new test
(define (dist-test x y z)
  (define (report-failure r1 r2)
    (display "Failed distributing ")
    (print-interval x)
    (display " over ")
    (print-interval y)
    (print-interval z)
    (display ": ")
    (print-interval r1)
    (display " expected but got ")
    (print-interval r2)
    )
  ; This compares X * (Y + Z) to (X * Y) + (X * Z)
  (let ((r1 (mul-interval x (add-interval y z) ))
        (r2 (add-interval (mul-interval x y) (mul-interval x z)))
        )
    (test-report (lambda() (eql-interval r1 r2)) (lambda() (report-failure r1 r2)))
    )
  )


; First ensure that our distributive test works correctly

; Intervals with zero-width compute as reals,
; and thus do distribute properly.
(define real1 (make-interval 7 7))
(define real2 (make-interval -1 -1))
(define real3 (make-interval 4.5 4.5))
(display "Checking distribution with 'reals' ...")
(dist-test real1 real2 real3)

; This will fail
(define A (make-interval  9 11))
(define B (make-interval -1 1.5))
(define C (make-interval 5 5.5))
(display "Checking distribution with intervals ...")
(dist-test A B C)

; As for the last question, Affine transformations provide an improved form of interval arithmetic.  There are interval arithemetics that are distributive as well.

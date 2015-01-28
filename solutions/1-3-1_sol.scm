; Section 1.3.1

;(define (add1 x) (+ x 1)) 

(define square sqr)

(define (cube x) (* x x x))

(define (identity x) x)

(define (sum term a next b) 
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))
      )
  )

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx)) 
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx
     )
  )

; Ex 1.29
; Integration using Simpson's Rule

; Implement a numeric integral solver using Simpson's Rule, and compare it to the above method (which uses the Trapezoid Rule).

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (simp-term k)
    (cond
      ((or (<= k 0) (>= k n))
       (cond
         ((= k 0) (f a))
         ((= k n) (f b))
         (else 0)
         )
       )
      ((even? k) 
       (* 2.0 (f (+ a (* k h))))
       )
      (else 
       (* 4.0 (f (+ a (* k h))))
       )
      )
    )

  (* (/ h 3.0) (sum simp-term 0 add1 n))
  )


; Testing
(displayln "Testing integration using Simpson's Rule")
(displayln "Trapezoid Rule Results:")
(integral cube 0.0 1.0 (/ 1 100))
(integral cube 0.0 1.0 (/ 1 1000))
(define error-trapezoid (abs (- 0.25 (integral cube 0.0 1.0 (/ 1 1000)))))
(display "Error for dx = 1/1000: ")
(display error-trapezoid)
(newline)
(newline)
(displayln "Simpson's Rule Results:")
(integral-simpson cube 0.0 1.0 100)
(integral-simpson cube 0.0 1.0 1000)
(define error-simpson (abs (- 0.25 (integral-simpson cube 0.0 1.0 1000))))
(display "Error for  n =   1000: ")
(display error-simpson)
(newline)
(newline)
(displayln "Testing non-converging integrals")
; Testing an integral that does not converge.
(define (one-over-x x) (/ 1.0 x))
(integral one-over-x 0.0 1.0 (/ 1 1000))
(integral-simpson one-over-x 0.0 1.0 1000)

(define (one-over-one-plus-x x) (/ 1.0 (+ x 1.0)))
(integral-simpson one-over-one-plus-x  2.0 3.0 10)  ; 0.287682 (does converge in this region)
(integral-simpson one-over-one-plus-x  -1.5 -0.4 100)  ; does not converge


; Ex 1.30
; Iterative summing 

(newline)
(displayln "Comparing iterative vs. linear recursive integral methods")
(displayln "Recursive (original):")
(define steps 200000)
(time (integral cube 0 1.0 (/ 1 steps)))  ; for comparison

; Modify the sum procedure to be iterative rather than linear recursive.

(define (sum term a next b) 
  (define (iter a result)
    (if (> a b)
        result
        (iter  (next a) (+ (term a) result))
        )
    )
  (iter a 0)
  )   

(displayln "Iterative:")
(time (integral cube 0 1 (/ 1 steps))) 

; Ex 1.31
; Products as a higher order procedure

; a. Write an abstract (product) function, similar to sum.

(define (product f a next b)
  (define (iter a partial)
    (if (> a b)
        partial
        (iter (next a) (* (f a) partial))
        )
    )
  (iter a 1)
  )

; Define factorial using this product function

(define (factorial n)
  (if (> n 0)
      (product identity 1 add1 n)
      1
      )
  )

; Testing
(define (factorial-test)
  (displayln (factorial 1))
  (displayln (factorial 4))
  (displayln (factorial 0))
  (displayln (factorial 13))
  (time (void (factorial 10000))) ; large number, not displayed
  )

(newline)
(displayln "Testing factorial (iterative)")
(factorial-test)

; Approximate pi using a formula due to Wallis:
; pi/4 = 2*4*4*6*6*8*8... / 3*3*5*5*7*7...

(define (wallis-pi n)
  (define (term i)
    (if (even? i)
        (/ (+ 2 i) (+ 3 i))
        (/ (+ 3 i) (+ 2 i))
        )
    )
  (* 4.0 (product term 0 add1 (- n 1)))
  )

; Testing
(newline)
(displayln "Testing approximation to pi (iterative)")
(wallis-pi 100)
(wallis-pi 1000)
(time (wallis-pi 10000)) ; observed 750

; replace product with a recursive process

(define (product f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))
      )
  )

(newline)
(displayln "Testing factorial (recursive)")
(factorial-test)
(newline)
(displayln "Testing approximation to pi (recursive)")
(wallis-pi 100)
(wallis-pi 1000)
(time (wallis-pi 10000)) ; observed 6700


; Ex 1.32
; Generalizing product and sum 

; Write an accumulate procedure that can be used to define both sum and product simply.

(define (accumulate combiner null-value term a next b)
  (define (iter a partial)
    (if (> a b)
        partial
        (iter (next a) (combiner (term a) partial))
        )
    )
  (iter a null-value)
  )

; Define (sum) and (product) using accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b)
  )

(define (product term a next b)
  (accumulate * 1 term a next b)
  )
; Testing
(newline)
(displayln "Testing accumulate using (sum) and (product) (iterative/recursive)")
(define (test-accumulate)
  (displayln (sum identity 1 add1 10)) ; 55
  (displayln (product identity 3 add1 5)) ; 60  
  (displayln (integral square 0 1 (/ 1 1000))) ; 0.333...
  (displayln (factorial 100)) ; rather big number, starts with 9333262154...
)

(test-accumulate)

; b. If accumulate works iteratively, implement it recursively (or vice versa).

; Now, recursively:

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))
      )
  )

(displayln "Testing accumulate using (sum) and (product) (recursive/iterative)")
(test-accumulate)


; Ex 1.33
; Adding filtering to accumulate

; Create a 'filtered-accumulate' procedure that is similar to accumulate but takes a filter procedure that specifies a condition terms must meet to be combined.

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a partial)
    (if (> a b)
        partial 
        (if (filter a)
            (iter (next a) (combiner (term a) partial))
            (iter (next a) partial)
            )
        )
    )
  (iter a null-value)
  )

; Using filtered-accumulate, express :
; a. The sum of squares of prime numbers in an interval

; Required 'prime?' predicate
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
    )
  )
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

;

(define (sum-of-prime-squares a b)
  (filtered-accumulate + prime? 0 square a add1 b)
  )

; Testing
(newline)
(displayln "Testing filtered-accumulate (sum of squares of primes in a range)")
(sum-of-prime-squares 100 110) ; 44140
(sum-of-prime-squares 1 10) ; 88

; b. The product of all positive integers less than n that are relatively prime to n.

(define (prod-rel-prime n)
  (define (rel-prime? a b)
    (= (gcd a b) 1)
    )
  (define (rfilter a)
    (rel-prime? a n)
    )
  (filtered-accumulate * rfilter 1 identity 1 add1 (- n 1))
  )

; Testing
(displayln "Testing filtered-accumulate (product of numbers relatively prime and less than n)")
(prod-rel-prime 11)  ; 3628800 (=10!)
(prod-rel-prime 40)  ; 4873615036539089841
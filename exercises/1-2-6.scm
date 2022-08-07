; Section 1.2.6

;Define square (only necessary for Racket/PB)
(define square sqr)

; Support procedures
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


; Ex 1.21.
; Using smallest-divisor

; Find the answer returned for these calls:
(displayln "Finding smallest divisor")
(smallest-divisor 199)    
(smallest-divisor 1999)   
(smallest-divisor 19999)  

; Ex 1.22.
; Timed Prime Test

; Modified to use 'time' 
(define (timed-prime-test n)  
  (display n)
  (if (prime? n) 
      (report-prime n)
      (newline)
      )
  )

(define (report-prime p)
  (display " *** ")
  (time (prime? p))
  (void)
  )
  
; Testing prime test
(displayln "Verifying timed-prime-test")
(timed-prime-test 509)
(timed-prime-test 5623) 
(timed-prime-test (- (expt 2 31) 1)) ; Mersenne prime (2^19 also valid if this is too slow)

; Write a (search-for-primes)
; procedure that searches for primes within a range


; Testing search-for-primes
(displayln "Testing search-for-primes")
; modify if arguments are different
(search-for-primes 1 50)  
; Determine appropriate upper bound
(search-for-primes 1000 <?>)
(search-for-primes 10000 <?>)
(search-for-primes 100000 <?>)
(search-for-primes 1000000 <?>)

; Timing for modern (faster) computers:
; Test timing of multiple iterations
(define (timed-prime-test-with-cycles n count)
  (display n)
  (if (prime? n) 
      (report-prime-with-cycles n count)
      (newline)
      )
  )

(define (report-prime-with-cycles p count)
  (display " *** ")
  (time (test-prime-iter p count))
  (void)
  )

(define (test-prime-iter p count)
  (prime? p)
  (if (> count 1)
      (test-prime-iter p (- count 1))
      )
  )

(define test-cycles 1000)  ; Adjust this value depending on the computer's speed

; Example usage
;
; This will run the timed-test 1000 times, and report the total time taken.
; Verification:
(displayln "Using 1000 cycles:")  
(display "testing 519 : ")
(timed-prime-test-with-cycles 519 test-cycles)

; Ex 1.23.
; Improved version of (smallest-divisor)

; Modify (smallest-divisor) so that it uses a procedure
; (next) to get values instead of testing any even numbers > 2.



; Testing
(newline)
(displayln "Using improved smallest-divisor")
(displayln "testing 1000+") 
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)

(displayln "testing 10000+")  
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)

(displayln "testing 100000+")  
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)

(displayln "testing 1000000+")  
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)
(timed-prime-test-with-cycles <argument> test-cycles)

; Check whether the improved procedure runs twice as fast as the original, and explain why if it does not.


; Ex 1.24.
; Using the Fermat test 

(define (expmod base exp m) 
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
  )

(define (fermat-test n) 
  (define (try-it a)
    (= (expmod a n n) a)
    )
  (try-it (+ 1 (random (- n 1))))
  )


(newline)
(displayln "Using Fermat primality test")
(displayln "testing 1000+") 
<?? fill in test ??>

(displayln "testing 10000+")  
<?? fill in test ??>

(displayln "testing 100000+")  
<?? fill in test ??>

(displayln "testing 1000000+")  
<?? fill in test ??>

; What is the expected ratio in times for 1000000 compared to 1000 using this test? 


; Ex 1.25
; Alternative (expmod) procedure

(define (expmod base exp m) (remainder (fast-expt base exp) m))

(define (fast-expt b n) 
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
        )
  )
; Is there any problem using this definition for expmod?


; Verification
(newline)
(displayln "Verifying ALH expmod:") 

;Using ALH expmod method in Fermat primality test 
; Use the appropriate test
(display "testing 1000+ ")
<?? fill in test ??>

(display "testing 10000+ ")  
<?? fill in test ??>

(display "testing 100000+ ")  
<?? fill in test ??>

(display "testing 1000000+ ") 
<?? fill in test ??>

; Ex 1.26.
; A faulty version of (expmod)

(define (expmod base exp m) 
  (cond 
    ((= exp 0) 1)
    ((even? exp) (remainder (* (expmod base (/ exp 2) m)
                               (expmod base (/ exp 2) m)
                               )
                            m
                            )
                 )
    (else (remainder (* base (expmod base (- exp 1) m)) 
                     m
                     )
          )
    )
  )

; Verification
(newline)
(displayln "Verifying L. Reasoner's expmod.")
; Note: Only ONE test-cycle in the following
(display "testing 1000+ ")
(timed-prime-test <argument>)

(display "testing 10000+ ")  
(timed-prime-test <argument>)

(display "testing 100000+ ")  
(timed-prime-test <argument>)

(display "testing 1000000+ ") 
(timed-prime-test <argument>)   

; Ex 1.27.
; Carmichael numbers and the Fermat test

; restore expmod to properly working one.
(define (expmod base exp m) 
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
  )

; Write a procedure to test for Carmichael numbers, and verify that the Fermat test give false-positive results for these numbers.

  
;  Testing
(newline)
(displayln "Testing Carmichael numbers.")
; These are all Carmichael numbers
(??carmichael test?? 561)
(??carmichael test?? 1019)
(??carmichael test?? 1105)
(??carmichael test?? 1729)
(??carmichael test?? 2465)
(??carmichael test?? 2821)
(??carmichael test?? 6601)
; Optional extras
;(??carmichael test?? 15841)
;(??carmichael test?? 825265)
; These are not Carmichael numbers
(??carmichael test?? 525)
(??carmichael test?? 1000)
(??carmichael test?? 10037)
(??carmichael test?? 12345)

(newline)
(displayln "Testing primality of Carmichael numbers")
(displayln "Using Fermat test")
; All except non-primes report as primes using Fermat test
(timed-prime-test 2) ; true prime
(timed-prime-test 561)
(timed-prime-test 1000) ; non-prime, non-Carmichael
(timed-prime-test 1105)
(timed-prime-test 1729)
(timed-prime-test 10037) ; true prime
(timed-prime-test 12345) ; non-prime, non-Carmichael
(timed-prime-test 15841)
(timed-prime-test 100049) ; true prime
(timed-prime-test 825265)

; Ex 1.28.
; Miller-Rabin test for primality

; Modify expmod to check for 'nontrivial square roots of 1 mod n'.


; Testing
(newline)
(displayln "Using Miller-Rabin test")
; Only the true primes should report as primes
(timed-prime-test 2) ; true prime
(timed-prime-test 561)  
(timed-prime-test 1000) ; non-prime, non-Carmichael
(timed-prime-test 1105)
(timed-prime-test 1729)
(timed-prime-test 10037) ; true prime
(timed-prime-test 12345) ; non-prime, non-Carmichael
(timed-prime-test 15841)
(timed-prime-test 100049) ; true prime
(timed-prime-test 825265)



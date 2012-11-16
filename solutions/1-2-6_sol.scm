; Section 1.2.6

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
(smallest-divisor 199)    ; 199
(smallest-divisor 1999)   ; 1999
(smallest-divisor 19999)  ; 7

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

(define (search-for-primes start end)
  (if (<= start end)
      (search-valid-primes start end)
      )
  )

(define (search-valid-primes start end)
  (if (odd? start) (timed-prime-test start))
  (search-for-primes (+ 1 start) end)
  )

; Testing search-for-primes
(displayln "Testing search-for-primes")
(search-for-primes 1 50)
(search-for-primes 1000 1050)

; created function to find the nth prime > than a value

(define (search-for-nth-prime start n)
  (if (prime? start)
      (if (or (= n 1) (< n 1))
          start
          (search-for-nth-prime (+ 1 start) (- n 1))
          )
      (search-for-nth-prime (+ 1 start) n)
      )
  )


(search-for-nth-prime 1000 3)
(newline)
; time taken for individual is zero
(search-for-primes 10000 (search-for-nth-prime 10000 3))
(newline)
(search-for-primes 100000 (search-for-nth-prime 100000 3))
(newline)
(search-for-primes 1000000 (search-for-nth-prime 1000000 3))
(newline)
; time taken = 1 or 0 in all ranges


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

(define test-cycles 1000)

(displayln "Using 1000 cycles:")  

(display "testing 1000+ ")
(timed-prime-test-with-cycles 1009 test-cycles)
; ~ 12 ms
(display "testing 10000+ ")  
(timed-prime-test-with-cycles 10007 test-cycles)
; ~ 40 ms
(display "testing 100000+ ")  
(timed-prime-test-with-cycles 100003 test-cycles)
; ~ 140 ms
(display "testing 1000000+ ")  
(timed-prime-test-with-cycles 1000003 test-cycles)
; ~ 380 ms

; The results are in the proportions expected, suggesting that this test is indeed sticking closely to the order sqrt(n).  Given that the test involves a consistent number of branches and mathematical operations, it is not surprising to find such a close match.  

; Ex 1.23.
; Improved version of (smallest-divisor)

; Modify (smallest-divisor) so that it uses a procedure
; (next) to get values instead of testing any even numbers > 2.

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 
        3
        (+ n 2)
        )
    )
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))
    )
  )

; Testing
(newline)
(displayln "Using improved smallest-divisor")
(displayln "testing 1000+") 
(timed-prime-test-with-cycles 1009 test-cycles)
(timed-prime-test-with-cycles 1013 test-cycles)
(timed-prime-test-with-cycles 1019 test-cycles)
; ~ 9 ms
(displayln "testing 10000+")  
(timed-prime-test-with-cycles 10007 test-cycles)
(timed-prime-test-with-cycles 10009 test-cycles)
(timed-prime-test-with-cycles 10037 test-cycles)
; ~ 26 ms
(displayln "testing 100000+")  
(timed-prime-test-with-cycles 100003 test-cycles)
(timed-prime-test-with-cycles 100019 test-cycles)
(timed-prime-test-with-cycles 100043 test-cycles)
; ~ 79 ms
(displayln "testing 1000000+")  
(timed-prime-test-with-cycles 1000003 test-cycles)
(timed-prime-test-with-cycles 1000033 test-cycles)
(timed-prime-test-with-cycles 1000037 test-cycles)
; ~ 246 ms

; Check whether the improved procedure runs twice as fast as the original, and explain why if it does not.

; The speed-up is not as fast as 2x, probably because the calls to 'next' require more time than calls to the primitive (+) function.  Next involves an extra check to see if the number is equal to 2. On my computer it takes roughly 2/3 of the time (1.5x speed improvement).

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

(define (fast-prime? n times)
  (cond 
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1))) 
    (else false))
  )

(define (prime? n)
  (fast-prime? n 10)
  )

(newline)
(displayln "Using Fermat primality test (10 probes)")
(displayln "testing 1000+") 
(timed-prime-test-with-cycles 1009 test-cycles)
(timed-prime-test-with-cycles 1013 test-cycles)
(timed-prime-test-with-cycles 1019 test-cycles)
; ~160
(displayln "testing 10000+")  
(timed-prime-test-with-cycles 10007 test-cycles)
(timed-prime-test-with-cycles 10009 test-cycles)
(timed-prime-test-with-cycles 10037 test-cycles)
; ~200
(displayln "testing 100000+")  
(timed-prime-test-with-cycles 100003 test-cycles)
(timed-prime-test-with-cycles 100019 test-cycles)
(timed-prime-test-with-cycles 100043 test-cycles)
; ~340
(displayln "testing 1000000+")  
(timed-prime-test-with-cycles 1000003 test-cycles)
(timed-prime-test-with-cycles 1000033 test-cycles)
(timed-prime-test-with-cycles 1000037 test-cycles)
; ~430

; What is the expected ratio in times for 1000000 compared to 1000 using this test? 

; It's expected that the ratio from 1000000 to 1000 to be 2:1 based on the Fermat test's scaling alone.  As it is, the time taken for an individual expmod run depends slightly on the size of the numbers used, though not by much.  The main factor is likely that expmod is not tail-recursive, as it must wait for the result.  This results in a time taken that is not precisely scaled to log n.  This will vary based on the randomness of test; some runs may require more steps than another.

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

; She is correct in that it will work, but it will likely run much slower. The version of (expmod) currently in use reduces the size of the numbers involved to be less than m.  This provides a savings in space and time. This will be even more noticeable when the numbers are very large.

; Verification
(newline)
(displayln "Verifying ALH expmod:") 

;Using ALH expmod method in Fermat primality test 
; Note: Only ONE test-cycle in the following
(display "testing 1000+ ")
(timed-prime-test 1009)
; ~ 1 ms
(display "testing 10000+ ")  
(timed-prime-test 10009)
; ~ 50 ms
(display "testing 100000+ ")  
(timed-prime-test 100003)
; ~ 2 s
(display "testing 1000000+ ")  
;(timed-prime-test 1000003)
; ~ 90.0 s

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


; This replaces a single recursive call to (expmod) with two calls that each make two sub-calls themselves.  This results in a total of 2^(# of calls to expmod in earlier version), which was O(log n).  O(2^log n) => O(n).

; Verification
(newline)
(displayln "Verifying L. Reasoner's expmod.")
; Note: Only ONE test-cycle in the following
(display "testing 1000+ ")
(timed-prime-test 1009)
; ~ 20 ms
(display "testing 10000+ ")  
(timed-prime-test 10009)
; ~ 220 ms
(display "testing 100000+ ")  
(timed-prime-test 100003)
; ~ 2.5 s
(display "testing 1000000+ ")  
;(timed-prime-test 1000003)
; ~ 25.5 s

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

(define (carm-test n)
  (define (report-found)
    (display n)
    (display " may be a Carmichael number.")
    (newline)
    )
  (define (report-not-found)
    (display n)
    (display " is not a Carmichael number.")
    (newline)
    )
  (if (carm-iter n n) 
      (report-found)
      (report-not-found)
      )
  )

(define (carm-iter a n)
  (if (= a 0) 
      true
      (if (= (expmod a n n) (remainder a n))
          (carm-iter (- a 1) n)
          false
          )
      )
  )

;  Testing
(newline)
(displayln "Testing Carmichael numbers.")
; These are all Carmichael numbers
(carm-test 561)
(carm-test 1019)
(carm-test 1105)
(carm-test 1729)
(carm-test 2465)
(carm-test 2821)
(carm-test 6601)
(carm-test 15841)
;(carm-test 825265)

; These are not Carmichael numbers
(carm-test 525)
(carm-test 1000)
(carm-test 10037) 
(carm-test 12345)

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

(define (expmod base exp m) 
  (define (nontriv-check)
    (define num
      (expmod base (/ exp 2) m)
      )
    (define val 
      (remainder (square num) m)
      )
    (if (= val 1)
        (if (or (= 1 num) (= num (- m 1)))
            val
            0
            )
        val
        )
    )
  
  (cond 
    ((= exp 0) 1)
    ((even? exp) (nontriv-check))
    (else (remainder (* base (expmod base (- exp 1) m)) m))
    )
  )

(define (miller-rabin-test n) 
  (define (try-it a)
    (= (expmod a n n) a)
    )
  (try-it (+ 1 (random (- n 1))))
  )

(define (fast-prime? n probes)
  (cond 
    ((= probes 0) true)
    ((miller-rabin-test n) (fast-prime? n (- probes 1))) 
    (else false))
  )
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
(timed-prime-test-with-cycles 100049 test-cycles)


; Section 3.5.2

(display "Including library files...")
(include "library/streambasic.scm")
(include "library/stream_352.scm")
(display-line "done.")

(define pi 3.14159265358979) ; define if necessary

; Ex 3.53.
; Testing a stream

; Describe the elements of this stream without running:

(define s (cons-stream 1 (add-streams s s)))
; Expected : (1 2 4 8 16 ...)

; Verification
(newline)
(display-line "Verifying the elements of stream s")
(display-n-of-stream 6 s)

; Ex 3.54.
; (mul-streams) and factorials

; Define (mul-streams) analagous to (add-streams)

(define (mul-streams . streams)
  (apply stream-map (cons * streams))
  )

; Testing mul-streams
(newline)
(display-line "Testing mul-streams")
(define t1 (mul-streams integers integers))
(define expected-1 (list-to-stream '(1 4 9 16 25 36 49 64 81 100)) )
(display "n*n:")
(stream-test = (limit-stream 10 t1) expected-1)

; Optional, allowing more than two arguments
; (define t2 (mul-streams integers integers s)) ; n^2*2^(n-1), testing multiple arguments
; Alternate test, combining binary operations
(define t2 (mul-streams (mul-streams integers s) integers))

(define expected-2 (list-to-stream '(1 8 36 128 400 1152 3136 8192 20736 51200)) )
(display "n^2*2^(n-1):")
(stream-test = (limit-stream 10 t2) expected-2) ; Testing multiple operands

; Define factorials using this function
(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(display-line "Testing factorials")
(define expected-f (list-to-stream '(1 2 6 24 120 720 5040 40320 362880 3628800 39916800)))
(display "factorials:")
(stream-test = (limit-stream 11 factorials) expected-f)
(newline)

; Ex 3.55.
; Partial sums

(define (partial-sums s)
  (cons-stream 
   (stream-car s)
   (add-streams (stream-cdr s) (partial-sums s))
   )
  )


; Testing
(newline)
(display-line "Testing partial sums")
(display-n-of-stream 7 (partial-sums integers))

; Do some checks
(display "Sum of integers:")
(stream-test = (limit-stream 10 (partial-sums integers)) (list-to-stream '(1 3 6 10 15 21 28 36 45 55)) )
(display "Sum of ones:")
(stream-test = (limit-stream 300 (partial-sums ones)) (limit-stream 300 integers))

; Ex 3.56.
; Creating a special sequence of Hamming's

; Enumerate in order the positive integers whose only prime factors are 2, 3, or 5

(define S (cons-stream 1 (merge (scale-stream S 5)  (merge (scale-stream S 2) (scale-stream S 3 )))))

; Testing
(newline)
(display-line "Testing Hamming's sequence")
;(display-n-of-stream 20 S)
(stream-test = (limit-stream 20 S) (list-to-stream '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)) )

(display "Testing 999th term:")
(equal? 51200000 (stream-ref S 999))
(newline)

; Ex 3.57.
; Additions under Fibonacci
(display "Checking addition count in Fibonacci sequence")
(define fibs-counted 
  (cons-stream 0
               (cons-stream 1
                            (stream-map add-counted 
                                        (stream-cdr fibs-counted) 
                                        fibs-counted
                                        )
                            )
               )
  )

; Verification
(define ac 0)
(define showcount true)
(define (add-counted x y)
  (set! ac (+ ac 1))
  (if showcount
      (begin
        (display ac)
        (display " additions")
        (display "(")
        (display x)
        (display "+")
        (display y)
        (display ") ")
        )
      )
  (+ x y)
  )

(stream-ref fibs-counted 7)
(display "addition count is ")
(display-line ac)

; The calls (internal to stream-map/add-streams) will result in repeated additions to determine
; the n-2nd and n-1st Fibonacci number separately, as the streams won't re-use any values.  The eventual result
; internally calculates the stream for n-2 and n-1, plus another n-1 additions to determine the result. 

; For the nth Fibonacci number (n>1), the additions will be: the additions to create fib(n-2) + additions to fib(n-1) + n-1 [to add the streams] 
; Thus the number of additions will grow about as fast as the Fibonacci sequence (in fact faster), which is > exponential growth.
(define fibc-additions (cons-stream 0 (cons-stream 0
                                             (add-streams integers
                                                          (add-streams fibc-additions
                                                                       (stream-cdr fibc-additions)
                                                                       )
                                                          )
                                             )
                              )
  )

(set! showcount false)
(set! ac 0)
(display-line "Additions required (without memoization):")
(display-n-of-stream 21 fibc-additions) ; Set this index to be one more since one additional fib number will be calculated in display-n-of-stream
(display-line "Sequence created:")
(display-n-of-stream 20 fibs-counted)
(display "Addition count: ")
(display ac)
(newline)

; Ex 3.58.
; Describing a stream

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)
   )
  )

; This gives the continued fraction, or "base radix" expansion of the rational number num/den.

; Each digit of the stream is a digit of the expansion.
; Thus (expand 1 7 10) should be 1/7 in base 10, or the decimal expansion = 0.142857...

; Verification
(newline)
(display-line "Verifying the stream behavior")

(display-n-of-stream 12 (expand 1 7 10)) ; 0.142857142857
(display-n-of-stream 6 (expand 3 8 10)) ; 0.375000

; Additional values
(display-n-of-stream 4 (expand 1 3 9))      ; 0.3000
(display-n-of-stream 4 (expand 3 8 16))     ; 0.6000 
(display-n-of-stream 9 (expand 501 511 8))  ; 0.765765765...


; Ex 3.59
; Power Series

; a. Define integrate-series that will integrate a power series (less the constant c)
(define (integrate-series pow_s)
  (mul-streams (stream-map / ones integers) pow_s)
  )

; b. Since e^x can be generated by:
(define exp-series (cons-stream 1 (integrate-series exp-series)))

; Define sine and cosine as power series similarly.

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)) ; using 1 gives us rational instead of floating point values
  ; it's easier to compare to the exp-series
  )

(define sine-series
  (cons-stream 0 (integrate-series cosine-series))
  )

; Verifying
(newline)
(display-line "Verifying exponential, sine, and cosine series")
(display "exp-series:")
(display-n-of-stream 10 exp-series)
(display "sine-series:")
(display-n-of-stream 10 sine-series)  ; (Note that sine-series = even terms of exp-series with alternating signs)
(display "cosine-series:")
(display-n-of-stream 10 cosine-series) ; (Cosine-series = odd terms of exp-series with alternating signs)

; Additional functions for power series
; Bear in mind that evaluation only works in the radius of convergence
(define (powers-of-x x)
  (cons-stream 1 (scale-stream (powers-of-x x) x))
  )

(define (eval-at x ps)
  (mul-streams ps (powers-of-x x))
  )

(define (accum-n-of-stream n s)
  (let ((acc 0)
        )
    (for-each (lambda (x) (set! acc (+ acc x))) (get-n-of-stream n s))
    acc
    )
  )

(display-line "Evaluating power series (100 terms):")
(display "e^1: ")
(accum-n-of-stream 100 (eval-at 1.0 exp-series))
(display "sin(pi/6): ")
(accum-n-of-stream 100 (eval-at (/ pi 6) sine-series))
(display "cos(pi/3): ")
(accum-n-of-stream 100 (eval-at (/ pi 3) cosine-series))


; Ex 3.60.
; Series multiplication

; Note: this only works on infinite streams

(define (mul-series s1 s2) 
  (cons-stream 
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (add-streams  (scale-stream (stream-cdr s2) (stream-car s1))
                  (scale-stream (stream-cdr s1) (stream-car s2))
                  )
                  (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                  )
   )
    )

; Alternate version
;(define (mul-series s1 s2)
;  (cons-stream (* (stream-car s1) (stream-car s2))
;               (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 
;                                        (mul-series (stream-cdr s1) s2))))


; Testing
(newline)
(display-line "Testing series multiplication")

(define pythag-identity (add-streams (mul-series cosine-series cosine-series) (mul-series sine-series sine-series)))

(display "sin^2 + cos^2 = ")
(display-n-of-stream 8 pythag-identity)

(display "(n) * (n) = ")
(display-n-of-stream 8 (mul-series integers integers)) ; Tetrahedral numbers - 1 4 10 20 35 56 84 120


; Ex 3.61.
; Multiplicative Inverse of Unit-series

(define (invert-unit-series us)
  (if (not (= 1 (stream-car us)))
      (error "INVERT-UNIT-SERIES : Series does not start with 1 : " us)
      (cons-stream
       1
       (scale-stream (mul-series (invert-unit-series us) (stream-cdr us)) -1)
       )
      )
  )

; Testing
(define sec-series (invert-unit-series cosine-series))
(define csc-series (invert-unit-series (stream-cdr sine-series))) ; offset due to first term being 0

(newline)
; Check that inverse can be multiplied to equal 1
(display-line "Testing (invert-unit-series)")
(display "Secant terms: ")
(display-n-of-stream 10 sec-series)
(display "Sec * cos: ")
(display-n-of-stream 10 (mul-series cosine-series sec-series)) ; Expect = 1
(newline)

; Alternating powers of a number x = 1/(1+x)
(display-line "(-x)^n = 1/(1+x):")
(define 3powers-alternating (powers-of-x -3))
(display "(-3)^n : ")
(display-n-of-stream 10 3powers-alternating)
(display "Inverse: ")
(display-n-of-stream 10 (invert-unit-series 3powers-alternating))
; Expected result stream is (1 3 0 0 ...) - try changing the base to another value

(newline)
(display-line "csc 2x = 1/2(sec x * csc x)")
(display "csc 2x            : ")
(display-n-of-stream 10 csc-series)
; Note that (eval-at) for csc-series we have only works in this specific case, since 'csc-series' is not properly the power series.
; It is offset by one term, but since we are "evaluating" at 1/2 and also scaling by 1/2, it works.
(display "1/2(sec x * csc x): ")
(display-n-of-stream 10 (mul-series (eval-at (/ 1 2) csc-series) (eval-at (/ 1 2) sec-series)) )

; For comparison
;(define csc-at-pi4 (scale-stream (invert-unit-series (scale-stream (stream-cdr (eval-at (/ pi 4) sine-series)) (/ 4 pi))) (/ 4 pi)))
;(define csc-at-pi8 (scale-stream (invert-unit-series (scale-stream (stream-cdr (eval-at (/ pi 8) sine-series)) (/ 8 pi))) (/ 8 pi)));
;(display-n-of-stream 10 (scale-stream csc-at-pi4 2.0))
;(display-n-of-stream 10 (mul-series csc-at-pi8 (eval-at (/ pi 8) sec-series) ))

; Ex 3.62.
; Division of series

(define (div-series dividend-s divisor-s)
  (let ((divconst (stream-car divisor-s)))
    (if (= 0 divconst)
        (error "DIV-SERIES : Divisor has constant 0 term: " divisor-s)
        (mul-series (scale-stream dividend-s divconst)
                    (invert-unit-series (scale-stream divisor-s (/ 1 divconst)))
                    ) 
        )
    )
  )

; Testing
(newline)
(display-line "Testing div-series with tangent")
(define tangent-series (div-series sine-series cosine-series))
(display "Tangent series: ")
(display-n-of-stream 10 tangent-series)


(display "Tangent, evaluated at pi/4 (100 terms): ")
(accum-n-of-stream 100 (eval-at (/ pi 4) tangent-series)) ; Expected value of 1.0

; Demonstration of other series division

; Multiplying (1 - x) by the integer stream gives a value of 1 for each term, so that is our expected result from division.
(define ones-over-ints (div-series ones integers))
(display "Ones divided by integers (series division):")
(display-n-of-stream 10 ones-over-ints) ; Expected: (1 -1 0 0 0 ...) 
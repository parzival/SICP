; Section 3.5.3

(display "Including library files...")

(define lib-path "library/") ; REDEFINE AS NEEDED

; Load-from-lib for MIT Scheme and most others
(define (load-from-lib filename)
  (load (string-append lib-path filename))
  )

; Load-from-lib for Racket/Pretty Big or similar
;(define (load-from-lib filename)
;  (load (string->path (string-append lib-path filename)))
;  )


; Measure-time is defined as a special form. Given a procedure,
; it will execute it and instead of returning the value, report the time taken

; Uncomment based on version desired:

; Measure-time for Racket/Pretty Big
;(define-syntax measure-time
;  (syntax-rules ()
;    ((_ thunk)
;      (time (void thunk))
;      )
;    )
;  )


; Measure-time for MIT Scheme

; MIT-Scheme's 'with-timings' needs a formatting function
(define (format-time-output run-time gc-time real-time)
   (display "time - cpu: ")
   (write (internal-time/ticks->seconds run-time))
   (display " gc: ")
   (write (internal-time/ticks->seconds gc-time))
   (display " real: ")
   (write (internal-time/ticks->seconds real-time))
  )

; Special form for measuring time
(define-syntax measure-time
  (syntax-rules ()
    ((_ thunk)
       (with-timings
         (lambda () thunk 'done) 
         format-time-output
         )
       )
     )
  )

; End of Implementation-specific functions


(load-from-lib "stream_353.scm")
(display-line "files finished.")

; Possible math functions needed

(define pi 3.14159265358979) ; define if necessary

;(define square sqr) ; give alternate name for squaring function if needed

; Original Math functions, from 1.1.7
(define (average x y)
  (/ (+ x y) 2)
  )

(define (old-sqrt-improve guess x) 
  (average guess (/ x guess))
  )



; Ex. 3.63.
; Local variables and the efficiency of streams


; Original
(define (sqrt-stream x) 
  (define guesses
    (cons-stream 1.0 (stream-map (lambda (guess)
                                   (old-sqrt-improve guess x)
                                   )
                                 guesses
                                 )
                 )
    )
  guesses
  )

; Verification
(newline)
(display-line "Verifying sqrt-stream")
(display-line-n-of-stream 12 (sqrt-stream 2))

(newline)
(display-line "Comparing time taken for two versions")
; Count the calls to the sqrt-improve function to compare efficiency

(define (sqrt-improve guess x)
  (set! call-count (+ call-count 1))
  (old-sqrt-improve guess x)
  )

(define call-count 0)

(define (show-call-count)
  (display "Call count is: ")
  (display call-count)
  (newline)
  )

(display-line "Original version")
(define s3 (sqrt-stream 13))

(measure-time (get-n-of-stream 1000 s3)) ; Specially defined for Racket & MIT-Scheme, see above
(show-call-count)

(display-line "Repeated:")
(set! call-count 0)
(measure-time (get-n-of-stream 1000 s3)) ; Specially defined for Racket & MIT-Scheme, see above
(show-call-count)

; Louis's simplified iterating sqrt-stream
(define (sqrt-stream x) 
  (cons-stream 1.0
               (stream-map 
                (lambda (guess) (sqrt-improve guess x))
                (sqrt-stream x)
                )
               )
  )

; If this approach is used, (sqrt-stream) is called as a function each time
; the cdr of the stream is forced.  This will completely re-evaluate the stream. Using
; memoization allows the original version to make calls against the same stream, which
; has already calculated its values.

; Verifying answer (compare to previous time value)
(newline)
(display-line "Louis's version")
(define sl3 (sqrt-stream 3))

(set! call-count 0)
(measure-time (get-n-of-stream 1000 sl3)) 
(show-call-count)

(display-line "Repeated:")
(set! call-count 0)

(measure-time (get-n-of-stream 1000 sl3))
(show-call-count)


; Ex 3.64.
; Limiting streams with tolerance

(define (stream-limit str tol)
  (define (compare-stream-to val str)
    (if (stream-null? str)
        (display-line "STREAM-LIMIT: Stream ended while still out of tolerance.")
        (let ((next-val (stream-car str)))
          (if (< (abs (- next-val val)) tol)
              next-val
              (compare-stream-to next-val (stream-cdr str))
              )
          )
        )
    )
  (if (stream-null? str)
      (error "STREAM-LIMIT: Stream is empty.")
      (compare-stream-to (stream-car str) (stream-cdr str))
      )
  )

; Testing


(newline)
(display-line "Testing (stream-limit)")

; Creates a geometric series
(define (make-geometric ival growth-rate)
  (define g
    (cons-stream ival
                 (scale-stream g growth-rate)
                 )
    )
  g
  )

(define ts-short (list-to-stream '(5 3 2 1.5 1.0))) ; short, simple stream
(define ts-decay  (make-geometric 10 -0.8)) ; decaying 
 
(define ts-wide (stream-enumerate-interval 1 10)) ; Stays out of tolerance
(define ts-grow (make-geometric 1.0 -2.0)) ; growing
(define ts-single (cons-stream 1 '()) )  ; only one element

(define ts-varying (list-to-stream (list 5 7 9 13 17 25 21 16 12 7 4 2 3 4 3 3.7 3.2 3.5 4 2 6 0 10))) ; First grows, then shrinks, then goes out of tolerance 

(display-line "Value shown is first one out of tolerance (all tol = 1.0 in this set)")

(display "Short, finite stream: ")
(stream-limit ts-short 1.0) ; 1.5, not before or after
(newline)

(display "Decaying infinite alternating stream: ")
(stream-limit ts-decay 1.0) ; Decays by term 14  (~= 0.43980 ), interpreting as "absolute value of the difference"
(newline)

(display "Finite stream with section within tolerance: ")
(stream-limit ts-varying 1.0) ; Reaches tolerance at term 14-15 (3.7)
(newline)

; Streams failing
(newline)
(display-line "Streams that fail limit test")
(display "Only one element: ")
(stream-limit ts-single 1.0) ; Stream only has one element
(display "Finite stream with all terms out of tolerance: ")
(stream-limit ts-wide 1.0) ; Stream remains out of tolerance.

;(display "Infinite stream, never in tolerance") 
;(stream-limit ts-grow 1.0) ; Infinite stream, never reaches tolerance.

(newline)
(display-line "Verifying sqrt using (stream-limit)")

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
  )

(display "Square root of 13 (tol = 0.00001): ")
(sqrt 3 0.00001)
(display-line "Square root of 2 =~ 1.41421356237309504880")
(display "Square root of 2 (tol = 5): ")
(sqrt 2 5)
(display "Square root of 2 (tol = 0.005): ")
(sqrt 2 0.005)
(display "Square root of 2 (tol = 0.0000001): ")
(sqrt 2 0.0000001)


; Ex 3.65.
; Approximating ln 2

(define (ln-2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-2-summands (+ n 1)))
               )
  ) 

; Alternate approach (executes slightly faster)
(define ln-2-stream (partial-sums (stream-map (lambda (n) (if (even? n) (- (/ 1.0 n)) (/ 1.0 n))) integers)))

;(define ln-2-stream (partial-sums (ln-2-summands 1)))
(define ln-2-stream-2 (euler-transform ln-2-stream))
(define ln-2-stream-3 (accelerated-sequence euler-transform ln-2-stream))


;Testing
(newline)
(display-line "Testing ln(2) approximation")
;
; A more precise value: 0.693147180559945309417232121458
(define ln2 0.6931471805599453)
(display "ln(2) =~ ")
(display ln2)
(newline)

; Method 1 (straight sequence of sums)
(display-line "Method 1 (original sequence, 15 terms)")
(display-line-n-of-stream 15 ln-2-stream)

; Method 2 
(display-line "Method 2 (Euler acceleration, 15 terms)")
(display-line-n-of-stream 15 ln-2-stream-2)

; Method 3
(display-line "Method 3 (Tableau acceleration, 15 terms)")
(display-line-n-of-stream 15 ln-2-stream-3)

; Comparing time to reach a given tolerance

(define ln-tol 0.001)

(display "Comparing terms required for identical tolerance (")
(display ln-tol)
(display ")")
(newline)


(define (test-ln2-diff x)
  (< (abs (- x ln2)) ln-tol)
  )

(display "Method 1 :")
(display (find-first-in-stream test-ln2-diff ln-2-stream)) ; first method
(display-line " terms.")

(display "Method 2 :")
(display (find-first-in-stream test-ln2-diff ln-2-stream-2)) ; second method
(display-line " terms.")

(display "Method 3 :")
(display (find-first-in-stream test-ln2-diff ln-2-stream-3)) ; third method
(display-line " terms.")


(define ln-tol 0.0000005)
(display "Comparing accelerated methods - tighter tolerance (")
(display ln-tol)
(display ")")
(newline)

; First method will likely take far too long, avoid testing

(display "Method 2 :")
(display (find-first-in-stream test-ln2-diff ln-2-stream-2)) ; second method (Euler)
(display-line " terms.")

(display "Method 3 :")
(display (find-first-in-stream test-ln2-diff ln-2-stream-3)) ; third method (Tableau)
(display-line " terms.")


; Ex 3.66.
; Looking at output of (pairs)

(define int_pairs (pairs integers integers))
(display-n-of-stream 200 int_pairs)

; Number of pairs to reach (1,100)
; Using 0-indexing, as with stream-ref
; (1,j) for j=1 is at 0
;       for j>1 will be at (2j-3) 

; (2,j) for j=2 is at (2^2 - 2)
;           j>2 has a period of 4 starting at 4 (2^2 + 2^1 - 2) => (4*(j-3)+4) = (4j-8) 

; (3,j) for j=3 is at (2^3 - 2)
;           j>3 has period 8 starting at 10 (2^3 + 2^2 - 2) => (8*(j-4)+10) = (8j-22)

; (4,j) for j=4 is at (2^4 - 2)
;           j>4 has period 16 starting at 22 (2^4 + 2^3 - 2) => (16*(j-5)+22) = (16j-58)
        

; (5,j) for j=5 is at (2^5 - 2)
;           j>5 has period 32 starting at 46 (2^5 + 2^4 - 2) => (32*(j-6)+46) = (16j-146)


; To reach (i,j)
; When j>i, the period repeats every 2^i, and starts at 2^i + 2^(i-1) - 2
;
; (i,j) for j=i will be at (2^i-2) 
;       for j>i will be at (2^i)*(j - i) + (2^(i-1)) - 2


; (99,100) and (100,100)

; Hence (99,100) will be term (2^99) + (2^98) - 2
; (100,100) is at 2^100-2. 


; Verification
(newline)
(display-line "Displaying results of (pairs integers integers)")

; Determine what index will give the desired results.
(display "Expected - (1 100): ")
(display-line (stream-ref int_pairs 197)) ; (1 100)
; Try more feasible values then (99,100), e.g. (9,10) and (10,10)
(display "Expected - (9 10): ")
(display-line (stream-ref int_pairs 766)) ; (9 10)= 2^9 + 2^8 - 2 = 766
(display "Expected - (10 10): ")
(display-line (stream-ref int_pairs 1022)) ; (10 10) = 2^10 - 2 = 1022


; Ex 3.67.
; Creating all pairs of integers

(define (all-pairs s t) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
     )
    (all-pairs (stream-cdr s) (stream-cdr t))
    )
   )
  )

; Testing
(newline)
(display-line "Verifying generation of all pairs of integers")
(display-n-of-stream 20 (all-pairs integers integers))


; Ex 3.68.
; Building streams naively

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (louis-pairs (stream-cdr s) (stream-cdr t))
   )
  )

; This fails since pairs does not delay any of its arguments, which leads to
; infinite recursion when the arguments to interleave are evaluated

; Verification
;(louis-pairs integers integers) ; does not return

; Ex 3.69.
; Triples from infinite streams

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda(p) (cons (stream-car s) p)) (pairs (stream-cdr t) (stream-cdr u)))  
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
    )
   )
  )

; Testing
(newline)
(display-line "Verifying triples (using integers):")
(display-n-of-stream 10 (triples integers integers integers))

(display-line "Verifying triples (using all integers, evens, and odds)")
(display-n-of-stream 10 (triples integers
                                 (stream-filter even? integers)
                                 (stream-filter odd? integers)
                                 )
                     )

(define stream-count 0)
; Pythagorean triples
(define pythag-triples (stream-filter (lambda(li) 
                                          (= (square  (caddr li)) (+ (square (car li)) (square (cadr li)))) 
                                          )
                                      (triples integers integers integers)
                                      )
  )


(display-line "Pythagorean triples:")
(display-n-of-stream 3 pythag-triples)
; Note that this may take quite a long time to process or even run out of memory
;(display-n-of-stream 6 pythag-triples)

; Ex 3.70.
; Ordered sequencing

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1) 
        (else
         (if (< (weight(stream-car s1)) (weight(stream-car s2)))
             (cons-stream (stream-car s1) (merge-weighted (stream-cdr s1) s2 weight))
             (cons-stream (stream-car s2) (merge-weighted s1 (stream-cdr s2) weight))
             )
         )
        )
  )

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight
    )
   )
  )

; Testing

(newline)
(display-line "Testing weighted ordering")

; a. Stream of positive integers i <= j, weighted by i+j

(define stream-a (weighted-pairs integers integers (lambda(p) (+ (car p) (cadr p)))))

(display-line "Positive integers with i <= j, weighted by sum:")
(display-n-of-stream 20 stream-a)
(display "Stream a correctly formed:")
(define expected-streama '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (2 4) (1 5) (3 4) (2 5) (1 6) (4 4) (3 5) (2 6) (1 7) (4 5) (3 6) (2 7) (1 8)))
(stream-test equal? (truncate-stream (length expected-streama) stream-a) (list-to-stream expected-streama) )
(newline)

; b. Stream of pairs of integers i <=j, where neither i nor j have 2,3 or 5 as factors,
; weighted by 2i+3j+5ij.


(define not-S235 (stream-filter (lambda (x)
                                  (not (or (= 0 (remainder x 2))
                                           (= 0 (remainder x 3))
                                           (= 0 (remainder x 5))
                                           )
                                       )
                                  )
                                integers
                                )
  )


(define stream-b (weighted-pairs not-S235 not-S235 (lambda(p)
                                               (let ((i (car p))
                                                     (j (cadr p))
                                                     )
                                                 (+ (* 2 i) (* 3 j) (* 5 i j))
                                                 )
                                               )
                                 )
  )

(display-line "Pairs with i <=j, where neither i nor j have 2,3,5 as factors, weighted by 2i+3j+5ij")
(display-n-of-stream 20 stream-b)
(display "Stream b correctly formed:")
(define expected-streamb '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49) (1 53) (7 11) (1 59) (1 61) (7 13)))
(stream-test equal? (truncate-stream (length expected-streamb) stream-b) (list-to-stream expected-streamb))
(newline)


; Ex 3.71.
; Hardy-Ramanujan numbers

(define (sum-cubes p)
  (+ (expt (car p) 3) (expt (cadr p) 3))
  )

(define cube-pairs (weighted-pairs integers integers sum-cubes))

; Apply filter, with entire stream as arg to predicate
; Intended for infinite streams, as this checks only at the start if stream is null; proc may need to check other elements.
(define (stream-map-s proc s)
  (if (stream-null? s) 
      the-empty-stream
      (cons-stream (proc s)
                   (stream-map-s proc (stream-cdr s))
                   )
      )
  ) 

(define (cubes-match? li)
   (= (sum-cubes (car li)) (sum-cubes (cadr li)))
  )

(define two-cube-sets (stream-map-s (lambda (s) (get-n-of-stream 2 s)) cube-pairs))

; If more than two in a row, there will be redundant values in the stream

(define hardy-ramanujan-stream (stream-map (lambda (l) (sum-cubes (car l))) (stream-filter cubes-match? two-cube-sets))) 

(newline)
(display-line "Finding Hardy-Ramanujan numbers.")
(display-line-n-of-stream 6 hardy-ramanujan-stream)

(display "Stream results match:")
(define expected-hr-numbers '(1729 4104 13832 20683 32832 39312 40033 46683 64232 65728 110656 110808 134379 149389 165464 171288 195841 216027 216125 262656 314496 320264))
(stream-test = (truncate-stream (length expected-hr-numbers) hardy-ramanujan-stream  ) (list-to-stream expected-hr-numbers))


; Ex 3.72.
; Sum of two squares in three ways

(define (sum-squares p)
  (+ (square (car p)) (square (cadr p)))
  )

(define (make-three-way-squares)

  (define (match-three-squares li)
     (= (sum-squares (car li)) (sum-squares (cadr li)) (sum-squares (caddr li)))
    )

  (define (make-three-pairs s)
    (get-n-of-stream 3 s)
    )

  (stream-filter match-three-squares
                 (stream-map-s make-three-pairs  (weighted-pairs integers integers sum-squares))
  )
  )
  
  
(newline)
(display-line "Showing sums of two squares written three ways:")
(define three-ways (make-three-way-squares))
(display-line-n-of-stream 10 three-ways)

; Testing against just the sums
(define three-way-sums (stream-map (lambda (el) (sum-squares (car el))) three-ways) )

; Note that there are more than 3 ways to produce 1105, so the test stops there.
(display "Sum of two squares in three ways is correctly produced:")
(stream-test = three-way-sums (list-to-stream '(325 425 650 725 845 850 925 1025 1105)) )
(newline)


; Ex 3.73.
; Modeling an RC circuit

(define (RC R C dt)
  (define (Vout i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) v0 dt)
                 )
    )
  Vout
  )

; Testing

(newline)
(display-line "Verifying RC circuit")
(define RC-timestep 0.5)
(define RC1 (RC 5 1 RC-timestep)) ; from text

(display-line "Constant current (0.1 A) - linearly increasing output expected")
(display-n-of-stream 20 (RC1 (scale-stream ones 0.1) 0))

(display-line "Charges, current shuts off after 5 secs")
(display-line "Expected: Voltage should increase linearly, then drop when current stops")
; Input rises to almost 1.0 V, drops to 0.5 V for 1/RC = 0.2
(define zeros (cons-stream 0 zeros))
(define temp-current (merge-weighted
                      (list-to-stream
                       (get-n-of-stream (inexact->exact (ceiling (/ 1.0  (* 0.2 RC-timestep))))
                                        (scale-stream ones 0.1)
                                        )
                       )
                      zeros
                      (lambda (x) (- x))
                      )
  )
(display "Input: ")
(display-n-of-stream 20 temp-current)
(display "Vout : ")
(display-n-of-stream 20 (RC1 temp-current 0))

(display-line "Initial voltage 1.0 V, constant current sink (-0.1 A) after one step")
(display-line "Expected: Voltage should have initial sharp drop, then decrease linearly")
(display-n-of-stream 20 (RC1 (cons-stream 0 (scale-stream ones -0.1)) 1.0))

(define (sin-stream amp freq phi step-size)
  (stream-map (lambda(x)
                (* amp (sin (+ (* freq  x) phi)))
                )
                (cons-stream 0
                             (scale-stream integers step-size)
                             )
                )
  )

(display-line "Sinusoidal current input, 0.2 A amplitude - output will follow 'behind' input slightly")
(define sine-current (sin-stream 0.1938 (/ pi 4) 0.245 RC-timestep))
(display "Input: ")
(display-n-of-stream 20 sine-current)
(display "Output: ")
(display-n-of-stream 20 (RC1 sine-current -0.247))


; Ex 3.74.
; Zero-crossing detector

(define (sign-change-detector current previous)
  ; These are true when the corresponding input is positive
  (let ((s1 (>= current 0))
        (s0 (>= previous 0))
        )
    (cond ((and s1 (not s0))
           1
           )
          ((and s0 (not s1))
           -1
           )
          (else
           0
           )
          )
    )
  )

; Alyssa's method
(define (make-zero-crossings input-stream last-value) 
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream)
                        )
   )
  )

; Since zero-crossings is not a procedure, we must define the sense-data stream first
(define sense-data-input '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define sense-data (list-to-stream sense-data-input))

(define zero-crossings-alyssa (make-zero-crossings sense-data 0))

(define zero-crossings-eva (stream-map sign-change-detector sense-data (cons-stream (stream-car sense-data) sense-data)))

; Verifying Zero Crossings
(newline)
(display-line "Zero Crossing Detector")

(display "Input:")
(display sense-data-input)
(newline)

(display "Alyssa:")
(display-n-of-stream (- (length sense-data-input) 1) zero-crossings-alyssa)

(display "Eva   :")
(display-n-of-stream (- (length sense-data-input) 1) zero-crossings-eva)
(newline)

(display-line "Sine wave with zero crossings (Alyssa):")

(define basic-sine (sin-stream 1 0.25 0 1))
(display-line "Input:")
(display-n-of-stream 44 basic-sine)

(display-line "Crossings:")
(define sin-crossings (make-zero-crossings basic-sine 0))
(display-n-of-stream 44 sin-crossings)


; Ex 3.75.
; Faulty averaging

(define (make-zero-crossings-louis input-stream last-value) 
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings-louis (stream-cdr input-stream) avpt)
                 )
    )
  )

; Verification
(newline)
(display-line "Verifying smoothing")


; Build a small 'high-frequency noise' signal (alternate positive & negative)
(define alt-sign (cons-stream 1 (cons-stream -1 alt-sign)))
(define noise-amplitude 0.35)
(define noise-stream (scale-stream alt-sign noise-amplitude))

(define noisy-sine (add-streams basic-sine noise-stream))

(display-line "Input (noisy sine wave):")
(display-n-of-stream 60 noisy-sine)

(display-line "Original zero-crossing method (no smoothing):")
(display-n-of-stream 60 (make-zero-crossings noisy-sine 0))

(display-line "Louis' zero-crossing method:")
(display-n-of-stream 60 (make-zero-crossings-louis noisy-sine 0))

; Error here is in averaging with "last-value" (averaging with the output) instead
; of averaging input values only.

(define (make-zero-crossings-fixed input-stream last-output last-input)
  (let ((avpt (/ (+ (stream-car input-stream) last-input) 2)))
    (cons-stream (sign-change-detector avpt last-output)
                 (make-zero-crossings-fixed (stream-cdr input-stream) avpt (stream-car input-stream))
                 )
    )
  )

(display-line "Fixed zero-crossing method:")
(display-n-of-stream 60 (make-zero-crossings-fixed noisy-sine 0 0))


; Ex 3.76.
; More modular averaging

(define (smoothing-function x1 x2)
  (/ (+ x1 x2) 2)
  )

(define (smooth input-stream)
  (cons-stream 
   (if (stream-null? (stream-cdr input-stream))
       the-empty-stream
       (smoothing-function (stream-car input-stream) (stream-car (stream-cdr input-stream)))
       )
   (smooth (stream-cdr input-stream))
   )
  )

; Verifying smooth

(display-line "Result of smoothing stream (noisy sine wave):")
(display-n-of-stream 60 (smooth noisy-sine))

; Smoothing shortens a finite stream by one (since there's nothing to average the
; last value with)
(display-line "Checking zero crossings with smooth (initial value padded with 0):")
(display-n-of-stream 60 (cons-stream 0 (make-zero-crossings (smooth noisy-sine) 0)))

(display-line "Smoothed sense-data")
(define sense-data-zero-crossings (make-zero-crossings (smooth sense-data) 0))
(display-n-of-stream (- (length sense-data-input) 2) sense-data-zero-crossings)

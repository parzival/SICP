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

; << define (stream-limit stream tolerance)  >>

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

; << define the original and accelerated streams >>

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
(display-line-n-of-stream 15 <<? stream 1 ?>>)

; Method 2 
(display-line "Method 2 (Euler acceleration, 15 terms)")
(display-line-n-of-stream 15 <<? Euler-accelerated ?>>)

; Method 3
(display-line "Method 3 (Tableau acceleration, 15 terms)")
(display-line-n-of-stream 15 <<? Tableau-accelerated ?>>)

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
(display (find-first-in-stream test-ln2-diff <<? stream 1 ?>>)) ; first method
(display-line " terms.")

(display "Method 2 :")
(display (find-first-in-stream test-ln2-diff <<? Euler-accelerated ?>>)) ; second method
(display-line " terms.")

(display "Method 3 :")
(display (find-first-in-stream test-ln2-diff <<? Tableau-accelerated ?>>)) ; third method
(display-line " terms.")


(define ln-tol 0.0000005)
(display "Comparing accelerated methods - tighter tolerance (")
(display ln-tol)
(display ")")
(newline)

; First method will likely take far too long, avoid testing

(display "Method 2 :")
(display (find-first-in-stream test-ln2-diff <<? stream 2 ?>> )) ; second method (Euler)
(display-line " terms.")

(display "Method 3 :")
(display (find-first-in-stream test-ln2-diff <<? stream32 ?>> )) ; third method (Tableau)
(display-line " terms.")


; Ex 3.66.
; Looking at output of (pairs)

(define int_pairs (pairs integers integers))
(display-n-of-stream 200 int_pairs)

; Verification
(newline)
(display-line "Displaying results of (pairs integers integers)")

; Determine what index will give the desired results.
(display "Expected - (1 100): ")
(display-line (stream-ref int_pairs <<? index ?>>)) ; (1 100)
; Try more feasible values then (99,100), e.g. (9,10) and (10,10)
(display "Expected - (9 10): ")
(display-line (stream-ref int_pairs <<? index ?>>)) ; (9 10)
(display "Expected - (10 10): ")
(display-line (stream-ref int_pairs <<? index ?>>)) ; (10 10)


; Ex 3.67.
; Creating all pairs of integers

; << define all pairs generator >>
; (It is recommended to use a different name rather than using 'pairs' so that
; the output can be compared with the first 'pairs' procedure)

; Testing
(newline)
(display-line "Verifying generation of all pairs of integers")
(display-n-of-stream 20 (<<? all pairs generator ?>> integers integers))


; Ex 3.68.
; Building streams naively

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (louis-pairs (stream-cdr s) (stream-cdr t))
   )
  )

; Verification
(louis-pairs integers integers) ; What happens?


; Ex 3.69.
; Triples from infinite streams

; << define triples >>
  
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
; << Use 'triples' to define the stream of pythagorean triples >>


(display-line "Pythagorean triples:")
(display-n-of-stream 3 <<? Pythagorean triples ?>> )
; Note that this may take quite a long time to process or even run out of memory
;(display-n-of-stream 6 <<? Pythagorean triples ?>> )


; Ex 3.70.
; Ordered sequencing

; << define merge-weighted >>

; << define weighted-pairs >>

; Testing

(newline)
(display-line "Testing weighted ordering")

; a. Stream of positive integers i <= j, weighted by i+j

; << define stream-a >>

(display-line "Positive integers with i <= j, weighted by sum:")
(display-n-of-stream 20 stream-a)
(display "Stream a correctly formed:")
(define expected-streama '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (2 4) (1 5) (3 4) (2 5) (1 6) (4 4) (3 5) (2 6) (1 7) (4 5) (3 6) (2 7) (1 8)))
(stream-test equal? (truncate-stream (length expected-streama) stream-a) (list-to-stream expected-streama) )
(newline)

; b. Stream of pairs of integers i <=j, where neither i nor j have 2,3 or 5 as factors,
; weighted by 2i+3j+5ij.

; << define stream-b >>

(display-line "Pairs with i <=j, where neither i nor j have 2,3,5 as factors, weighted by 2i+3j+5ij")
(display-n-of-stream 20 stream-b)
(display "Stream b correctly formed:")
(define expected-streamb '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49) (1 53) (7 11) (1 59) (1 61) (7 13)))
(stream-test equal? (truncate-stream (length expected-streamb) stream-b) (list-to-stream expected-streamb))
(newline)


; Ex 3.71.
; Hardy-Ramanujan numbers

; << define the stream >> 

(newline)
(display-line "Finding Hardy-Ramanujan numbers.")
(display-line-n-of-stream 6 <<? Hardy-Ramanujan stream ?>>)

(display "Stream results match:")
(define expected-hr-numbers '(1729 4104 13832 20683 32832 39312 40033 46683 64232 65728 110656 110808 134379 149389 165464 171288 195841 216027 216125 262656 314496 320264))
(stream-test = (truncate-stream (length expected-hr-numbers) <<? Hardy-Ramanujan stream ?>> ) (list-to-stream expected-hr-numbers))


; Ex 3.72.
; Sum of two squares in three ways

; << define the stream of numbers that are sum of two squares in three ways >>

(newline)
(display-line "Showing sums of two squares written three ways:")

(display-line-n-of-stream 10 <<? Stream showing numbers and how to produce them ?>>)

; Testing against just the sums
(define three-way-sums (stream-map (lambda (el) <<? extract just the sum ?>>) <<? defined stream ?>>) )

; Note that there are more than 3 ways to produce 1105, so the test stops there.
(display "Sum of two squares in three ways is correctly produced:")
(stream-test = three-way-sums (list-to-stream '(325 425 650 725 845 850 925 1025 1105)) )
(newline)


; Ex 3.73.
; Modeling an RC circuit

; << define RC >>

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

; Complete following code:

(define zero-crossings-eva (stream-map sign-change-detector sense-data <<?expression?>>))


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

; << define the fixed version >>

(display-line "Fixed zero-crossing method:")
(display-n-of-stream 60 (<<? fixed make-zero-crossings with noisy sine as input ?>>))


; Ex 3.76.
; More modular averaging


; <<? define smooth ?>>

; Verifying smooth

(display-line "Result of smoothing stream (noisy sine wave):")
(display-n-of-stream 60 <<? use smooth on noisy-sine ?>>)

; Smoothing shortens a finite stream by one (since there's nothing to average the
; last value with)
(display-line "Checking zero crossings with smooth (initial value padded with 0):")
(display-n-of-stream 60 (cons-stream 0 (make-zero-crossings <<? smooth noisy sine ?>> 0)))

(display-line "Smoothed sense-data")
(define sense-data-zero-crossings (make-zero-crossings <<? smooth sense-data ?>> 0))
(display-n-of-stream (- (length sense-data-input) 2) sense-data-zero-crossings)

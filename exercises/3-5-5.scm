; Section 3.5.5.

(display "Including files...")

(define lib-path "library/") ; REDEFINE AS NEEDED

; Load-from-lib for MIT Scheme and most others
(define (load-from-lib filename)
  (load (string-append lib-path filename))
  )

; Load-from-lib for Racket/Pretty Big or similar
;(define (load-from-lib filename)
;  (load (string->path (string-append lib-path filename)))
;  )

(load-from-lib "stream_353.scm")

(display-line "...file loading finished.")
(newline)

; Creates a stream from a given index to the final index, inclusive
(define (sub-stream start-index end-index str)
  (if (> start-index 0)
      (sub-stream (- start-index 1) (- end-index 1) (stream-cdr str))
      (truncate-stream (+ end-index 1) str)
    )
  )

; For random numbers, Racket/Pretty Big only needs this function.
; (random) and (make-pseudo-random-generator) are defined.
;(define (set-random-seed seed prg)
;  (let ((old-prg (current-pseudo-random-generator))
;        )
;    (current-pseudo-random-generator prg)
;    (random-seed seed)
;    (current-pseudo-random-generator old-prg)
;    )
;  )

; Define these for other interpreters, or modify as required

(define (make-pseudo-random-generator)
  (make-mwc-generator (remainder (current-jiffy) (expt 2 24)) (expt 2 23))
  )
 
(define (random prg)
  (prg 'generate)
  )
  
(define (set-random-seed seed prg)
  ((prg 'set-seed) seed)
  )

; A PRNG, usable if the Scheme interpreter does not have a built-in PRNG
(define (make-mwc-generator x0 c0)
  (let ((b (expt 2 24))
        (x x0)
        (c c0)
        (a 123)
        )
        
    (define (generate-unscaled) ; Gives the raw value (24-bit integer)
      (let ((xm (+ (* a x) c))
            )
        (set! x (remainder xm b))
        (set! c (quotient xm b))
        )
        x
      )
      
    (define (generate) ; Produces decimal values in [0,1)
        (exact->inexact (/ (generate-unscaled) b))
      )
      
    (define (set-seed nx)
      (if (>= 1.0 nx) ; Seed can be either integer or decimal in range [0,1)
        (set-seed-raw nx)
        (set-seed-raw (* b nx))
      )
    )
    
    (define (set-seed-raw nx)
      (if (< nx 0)
         (error "Random seed for mwc-generator may not be negative. Seed given was : " nx)
         (begin
           (set! x (remainder nx b))
           (set! c (if (= nx 0) c0 (quotient nx b)))
           )
         )
      )
      
    (define (reset)
      (set! x x0)
      (set! c c0)
      )
      
    (define (dispatch arg)
      (cond
        ((eq? arg 'generate)  (generate))
        ((eq? arg 'generate-raw) (generate-unscaled))
        ((eq? arg 'reset) (reset))
        ((eq? arg 'set-seed) set-seed)
        (else
         (error "Unknown argument for mwc-generator :" arg)
         )
        )
      )
    dispatch
    )
  )

; Define as needed
;(define square sqr)

(define pi 3.141592)


; Ex 3.81
; Repeatable random numbers from a stream

; From Ex 3.6, for reference :

;(define (rand arg)
;  
;  (define (generate) (random))
;  
;  (define (reset new-value) 
;    (random-seed new-value)  
;    )
;    
;  (cond
;    ((eq? arg 'generate)  (generate))
;    ((eq? arg 'reset) reset)
;    (else
;     (error "Unknown argument for rand :" arg)
;     )
;    )
;  )

; << define RNG using streams >>

; Testing 

; Produces an infinite stream of generate requests
(define constant-gen
  (cons-stream 'generate constant-gen)
  )

; Stream with given starting seed
(define (con-gen-with-seed r)
  (cons-stream (list 'reset r) constant-gen)
  )

; Used to convert abbreviated forms
(define (expand-abbrev-request req)
  (cond ((list? req) (map expand-abbrev-request req))
        ((eq? req 'r) 'reset)
        ((eq? req 'g) 'generate)
        (else req)
        )
  )

(define (make-request-stream li)
  (list-to-stream (map expand-abbrev-request li))
  )

; These are using abbreviated labels, but will be expanded to full-word requests
(define rng-test-requests (make-request-stream '((r 3) g g g (r 3) g g g (r 49) g g (r 593785) g (r 49) g g (r 54) g g)))

;(define rg-stream << stream output using rng-test-requests as input >> )

(display-line "Verifying reset and generate sequence")
(display-stream rg-stream)

(display-line "Testing generator produces same output after resets:")

; NOTE: Use this version if reset requests do NOT produce output.
;(display "Testing if reset and repeat works: ")
;(display (stream-test = (sub-stream 0 2 rg-stream) (sub-stream 3 5 rg-stream)))
;(newline)
;(display "Testing if second reset (with a different seed in-between) works: ")
;(display (stream-test = (sub-stream 6 7 rg-stream) (sub-stream 9 10 rg-stream)))
;(newline)
; END of tests relying on reset not producing output

; NOTE: Use this version if reset requests do produce output.
(display "Testing if reset and repeat works: ")
(display (stream-test = (sub-stream 0 3 rg-stream) (sub-stream 4 7 rg-stream)))
(newline)
(display "Testing if second reset (with a different seed in-between) works: ")
(display (stream-test = (sub-stream 8 10 rg-stream) (sub-stream 13 15 rg-stream)))
(newline)
; END of tests relying on reset producing output

(newline)
; Is stream ready to process from start?
(display-line "Checking stream can take any initial request")

; Create two streams using constant-gen as input stream 
;(define rs-congen-1 << One random stream using constant-gen as input >> )
;(define rs-congen-2 << Another random stream using constant-gen as input >> )

(display-line "Observing different streams starting with 'generate (match not required)")
(display-n-of-stream 4 rs-congen-1)
(display-n-of-stream 4 rs-congen-2)

; Create two streams using con-gen-with-seed as input stream (use the same seed for #)
;(define rs-congen-s-1 << One random stream using con-gen-with-seed as input >> )
;(define rs-congen-s-2 << Another random stream using con-gen-with-seed as input >> )

(display-line "Observing different streams starting with 'reset (match not required)")
(display-n-of-stream 4 rs-congen-s-1)
(display-n-of-stream 4 rs-congen-s-2)
(newline)

; Are stream values preserved properly? (memoization may play a role)
;(define rand-out-stream << Random stream using con-gen-with-seed as input >> )
(display-line "Observing stream-ref behavior")
(define y1 (stream-ref rand-out-stream 5))
(display "At index 2: ")
(display-line (stream-ref rand-out-stream 2))
(display "Repeated at 2: ")
(display-line (stream-ref rand-out-stream 2))
(display "At 500: ")
(display-line (stream-ref rand-out-stream 500))
(display "At 100: ")
(display-line (stream-ref rand-out-stream 100))
(define y2 (stream-ref rand-out-stream 5))

(display "Testing if stream-ref values remain consistent after access: ")
(display (eq? y1 y2))
(newline)

; Do rng streams interfere with each other?
(display-line "Observing two streams in concurrence")
(define rng-requests (make-request-stream '((r 51) g g g g g (r 51) g g g g g) ))
(define rng-requests-2 (make-request-stream '((r 12) g g (r 16) g g (r 12) g g) )) 

;(define nr-stream << Random stream using rng-requests as input >> )
;(define nr2-stream << Random stream using rng-requests-2 as input >> )

; NOTE: if 'reset' does NOT produce output, use this version
;(define r1s1  (stream-ref nr-stream 3))  ; reset +4
;(define r1s2  (stream-ref nr-stream 4))  ; reset +5
;(define r2s1  (stream-ref nr2-stream 0)) ; reset +1
;(stream-ref nr-stream 5) ; get to the reset command
;(define r2s2 (stream-ref nr2-stream 4))  ; reset +1
;(define r1s3  (stream-ref nr-stream 8)) ; reset +4
;(stream-ref nr2-stream 4)
;(stream-ref nr2-stream 0)
;(define r1s4 (stream-ref nr-stream 9)) ; reset +5
; END of test set-up relying on 'reset' not producing output

; NOTE: if 'reset' does produce output, use THIS version
(define r1s1  (stream-ref nr-stream 3))
(define r1s2  (stream-ref nr-stream 4))
(define r2s1  (stream-ref nr2-stream 1)) ; reset +1
(stream-ref nr-stream 6) ; the reset command
(define r2s2 (stream-ref nr2-stream 7))  ; reset +1
(define r1s3  (stream-ref nr-stream 9)) ; reset +3
(stream-ref nr2-stream 8)
(stream-ref nr2-stream 1)
(define r1s4 (stream-ref nr-stream 10)) ; reset +4
; END of test set-up relying on 'reset' producing output

(display-line "Testing if two streams interfere with each other")
(display "Stream 1 ok:")
(display (and (= r1s1 r1s3) (= r1s2 r1s4)))
(newline)

(display "Stream 2 ok:")
(display (= r2s1 r2s2))
(newline)

; Ex 3.82
; Monte Carlo integration with streams

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed)) 
                 (monte-carlo
                  (stream-cdr experiment-stream) passed failed
                  )
                 )
    ) 
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))
      )
  )

; << define pi-estimate as a stream >>

; Note that the RNG values need to be properly scaled to work in this

; Use unit circle: (lambda (x y) (< (+ (sqr x) (sqr y)) 1.0))
; Choose x-bounds and y-bounds with |x| and |y| > 1.0

; Testing
(newline)
(display-line "Using Monte Carlo integration to estimate pi (increasing number of iterations)")

(for-each (lambda (x) (display-line (stream-ref pi-estimate x))) '(10 100 1000 10000 100000) )

; Note: there is a small chance this will fail due to randomness
(display "Checking if pi estimate is close (within 0.05):")
(display (>= 0.05 (abs (- (stream-ref pi-estimate 100000)  pi))))

; Section 3.1.2.

; Both MIT and Chicken (or any non-Racket) will need some or all of these defined:

;      (define (displayln s)
;        (display s)
;        (newline)
;        )

; (define (allnull? ln)
;  (cond 
;     ((null? ln) #t)
;	 ((null? (car ln)) (allnull? (cdr ln)))
;	 (else #f)
; )
;)

;  (define (andmap p . ll )
;    (cond 
;      ((allnull? ll) 
;	    #t
;	    )
;	  ((apply p (map car ll)) ; true case
;	    (apply andmap (cons p (map cdr ll)))
;		)
;       (else #f)
;     )
;   )

;      (define (sqr x) (expt x 2))

;      (define pi 3.141592653589793)

; Ex 3.5.
; Monte Carlo integration

; Modified version to work with (random) in Racket for integer values.
(define (random-in-range low high)
  (let ((range (- high low)))
    ;  (+ low (random range)) <-- original version.
    (+ low (* range (random)))
    )
  )

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials)
           )
          ((experiment) 
           (iter (- trials-remaining 1) (+ trials-passed 1))
           )
          (else (iter (- trials-remaining 1) trials-passed))
          )
    )
  (iter trials 0)
  )

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((total-area (* (- x2 x1) (- y2 y1)))
        (hit-fraction (monte-carlo trials (lambda () 
                                            (P (random-in-range x1 x2)
                                               (random-in-range y1 y2)
                                               )
                                            )
                                   )
                      )
        )
    (* hit-fraction total-area)
    )
  )

; Testing
(displayln "Testing integral estimation (area of triangle)")

(define (monte-carlo-test trials estimator actual-value)
  (let ((result (estimator trials))
        )
    (display trials)
    (display " trials: ")
    (display result)
    (if showerror
        (begin
          (display "  % Error: ")
          (display (* 100 (abs (/ (- actual-value result) actual-value))))  
          )
        )
    (newline)
    
    )
  )

; Use (set! showerror true) [or false] to enable [disable] display of percentage error
(define showerror false)

; Triangle is a region bounded by y < 6x.  Test rectangle corners are (0,0) and (1,6)
(define triangleP (lambda (x y) (< y (* 6.0 x))))

(define (triangle-estimate test-trials) 
  (estimate-integral triangleP 0.0 1.0 0.0 6.0 test-trials)
  )

(monte-carlo-test 10000 triangle-estimate 3.0)

(newline)

; Tests using the unit circle (part of the exercise)
; Functions used for pi estimates with circles
(define circleP (lambda (x y)
                  (< (+ (sqr (- x xc)) (sqr (- y yc))) r_squared) 
                  )
  )

(define (pi-estimate trials)
  (/ (estimate-integral circleP
                        xleft
                        xright
                        ylower
                        yupper
                        trials
                        )
     r_squared
     )
  )



; Unit circle
(define xc 0.0)
(define yc 0.0) 
(define r_squared (* 1.0 1.0))

; Tight bounding box
(define xleft -1.0)
(define xright 1.0)
(define ylower -1.0)
(define yupper  1.0)

(displayln "Estimating pi using unit circle")

(define (pi-test trials)
  (monte-carlo-test trials pi-estimate pi)
  )

(for-each pi-test '(100 1000 10000))
(set! showerror true)
(pi-test 100000)
(set! showerror false)

; Large bounding box

(set! xleft -10.0)
(set! xright 10.0)
(set! ylower -10.0)
(set! yupper  10.0)

(displayln "Estimating pi using unit circle (large bounding box)")

(for-each pi-test '(100 1000 10000))
(set! showerror true)
(pi-test 100000)
(set! showerror false)

; Optional:
; Use circle defined in book: Center at (5,7), radius of 3

(displayln "Estimating pi using a different circle (tight bounding box)")
(set! xc 5.0)
(set! yc 7.0) 
(set! r_squared (* 3.0 3.0))
(set! xleft 2.0)
(set! xright 8.0)
(set! ylower 4.0)
(set! yupper 10.0)

(for-each pi-test '(100 1000 10000))
(set! showerror true)
(pi-test 100000)
(set! showerror false)
(newline)


; Ex 3.6.
; Reseeding random number generators

; Method 1: Using an LCG and a make-prng procedure.

; The LCG is of the form  a*x + c modulo m 
; If the values used for a and c match the following conditions, the sequence will cover all numbers less than m without repeating: 
; 1. m and c are relatively prime.
; 2. (a-1) is divisible by all prime factors of m.
; 3. (a-1) is divisible by 4 if m is divisible by 4.

(define (make-prng a b m s)
  (let ((random-seed s)
        )
    
    (define (rand-update)
      (set! random-seed (modulo (+ (* a random-seed) b) m))
      )
    
    (define (generate) 
      (rand-update)
      random-seed
      )  
    
    (define (reset new-value)
      (set! random-seed new-value) 
      )
    
    (define (dispatch arg)
    (cond
      ((eq? arg 'generate)  (generate))
      ((eq? arg 'reset) reset)
      (else
       (error "Unknown argument for rand :" arg)
       )
      )
      )
    dispatch
    )
  )

; Note the definition format - rand is the procedure returned by make-prng
(define rand 
  (make-prng  104877 139 209752 0)
  )

; Testing
(displayln "Verifying Random numbers.")

; Ensure that response is proper
(displayln "This should be two randomly generated values")
(rand 'generate)
(rand 'generate)
((rand 'reset) 0)
(displayln "Random seed has been reset.")

(define (add-n-rand-to-list n l)
  (if (<= n 0)
      l
      (add-n-rand-to-list (- n 1) (cons (rand 'generate) l))
      )
  )

(define (check-lists li1 li2)
  (if (andmap = li1 li2)
      (displayln "Lists match.")
      (displayln "Lists do not match.")
      )
  )

; Lists should match when the seed is the same.
(define rand-seed 1)
((rand 'reset) rand-seed)
(define lis1a (add-n-rand-to-list 100 '()))
((rand 'reset) rand-seed)
(define lis1b (add-n-rand-to-list 100 '()))
(displayln "Checking two lists with repeated seed (match expected):")      
(check-lists lis1a lis1b)

(set! rand-seed 50)
((rand 'reset) rand-seed)
(define lis50a (add-n-rand-to-list 100 '()))
((rand 'reset) rand-seed)
(define lis50b (add-n-rand-to-list 100 '()))
(displayln "Checking two lists with another seed (match expected):")
(check-lists lis50a lis50b)

; These lists are not expected to match
(displayln "Checking two lists produced with different seeds (match not expected):")
(check-lists lis1a lis50a)


; Method 2: Built-in procedures, external variable
; This is Racket/PB-only

(newline)
(displayln "Random reset using PRNG")

(define our-prng (make-pseudo-random-generator))

(define (rand arg)
  
  (define (generate) 
    (random our-prng)
    )  
  
  (define (reset new-value)    ; Reset using a PRNG
    (set! our-prng (vector->pseudo-random-generator (pseudo-random-generator->vector new-value))) ; This makes a copy of the PRNG
    )
  
  (cond
    ((eq? arg 'generate)  (generate))
    ((eq? arg 'reset) reset)
    (else
     (error "Unknown argument for rand :" arg)
     )
    )
  )


(define rand-seed (make-pseudo-random-generator))
((rand 'reset) rand-seed)
(define lis1a (add-n-rand-to-list 100 '()))
((rand 'reset) rand-seed)
(define lis1b (add-n-rand-to-list 100 '()))
(displayln "Checking two lists with repeated seed (match expected):")      
(check-lists lis1a lis1b)

(set! rand-seed (make-pseudo-random-generator))  ; Expected to produce a different PRNG
((rand 'reset) rand-seed)
(define lis50a (add-n-rand-to-list 100 '()))
((rand 'reset) rand-seed)
(define lis50b (add-n-rand-to-list 100 '()))
(displayln "Checking two lists with another seed (match expected):")
(check-lists lis50a lis50b)

(displayln "Checking two lists produced with different seeds (match not expected):")
(check-lists lis1a lis50a)



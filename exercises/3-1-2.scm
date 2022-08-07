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

;>> define estimate-integral

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

; >>Complete with proper arguments to the estimator 
(define (triangle-estimate test-trials) 
  (estimate-integral <<arguments>> )
  )

(monte-carlo-test 10000 triangle-estimate 3.0)

(newline)

; Tests using the unit circle (part of the exercise)

; >> Define the estimator function that is called with a given number of trials


; Tight bounding box
(define xleft -1.0)
(define xright 1.0)
(define ylower -1.0)
(define yupper  1.0)

(displayln "Estimating pi using unit circle")

; >> Replace with appropriate test function
(define (pi-test trials)
  (monte-carlo-test trials <<estimator>> pi)  
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

;(displayln "Estimating pi using a different circle (tight bounding box)")

; Adjust # of trials as desired
;(pi-test 100000)


; Ex 3.6.
; Reseeding random number generators

; >>define (rand)


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




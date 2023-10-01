; Section 3.5.4.

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

(load-from-lib "stream_353.scm")
(display-line "files finished loading.")
(newline)

; From text, integral using delay

; Note that delay/force will be defined based on 'streambasic' file.

(define (integral delayed-integrand initial-value dt) 
  (define int
    (cons-stream initial-value 
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt) int)
                   )
                 )
    )
  int
  )

; Differential equation solver 
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y
  )

; Initial verification
(display-line "Verifying integral (original version)")
(display "Integral of stream 'ones' (expected: whole numbers):")

(display-n-of-stream 10 (integral (delay ones) 0 1))  ; Should be the whole numbers


; Defining a common set of inputs

; Creates a stream of linearly-spaced floating-point values (for an integral or similar numeric function)
(define (point-stream dx)
  (scale-stream (cons-stream 0 integers) dx)
  )

; Converts a floating-point value to a numerical index 
(define (to-index x)
  (inexact->exact (round x))
  )

; Produces a properly spaced set of index values to check for math operations
; The input list is a set of numeric values. The output produced can be used on a stream as long as the step size matches.
; This allows us to vary the step size without having to change which values we are checking.
(define (generate-check-list step-size li)
  (map (lambda (x) (to-index (/ x step-size))) li)
  )


(define integral-step 0.01) ; step size used for all subsequent integral operations 

(define ps1 (point-stream integral-step))
(define integral-check-points (generate-check-list integral-step (list 0 0.5 1.0 1.5 2.0 2.5 3.0)))
; s1 : 3x^2 + 2x - 1
(define (f1 x) (+ (* 3 x x) (* 2 x) -1))
; s2 - expected result : x^3 + x^2 - x  (Constant set to 0 by initial value)
(define (int-f1 x) (+ (expt x 3) (expt x 2) (- x)))

(newline)
(display-line "Integral of a polynomial")
(display-line "s1 is the result of integral")
(display-line "s2 is the expected value (calculated from the solution function)")
(show-streams-at-points (integral (delay (stream-map f1 (point-stream integral-step))) 0 integral-step)
                        (stream-map int-f1 (point-stream integral-step))
                        integral-check-points
                        )



; Verification of solve function with delay
(define timestep 0.001)

(define euler-num 2.718281828459)

(newline)
(display-line "Verifying original solve, using calculation of e.")
(display "Known value of e to 12 decimal places: ")
(display euler-num)
(newline)
(display "Calculated value: ")
(define calced-e (stream-ref (solve (lambda (y) y) 1.0 timestep) (to-index (/ 1.0 timestep)) ) )
(display calced-e)
(newline)
(display "Difference is ")
(display (abs (- euler-num calced-e)))
(newline)

(newline)
(display-line "Verifying original solve, using another equation")
(define (f2 y) (+ (* 3 (expt y 2)) (* 2 y) 1))   ; f2: 3y^2 + 2y + 1
(define c0 (atan (* 2 (sqrt 2)))) ; Constant, giving initial value of 1.0
(define (fsol t) (/ (- (* (sqrt 2) (tan (+ (* (sqrt 2) t ) c0))) 1) 3.0) )  ; This is only valid up to (pi/2 - c0)/sqrt 2 ~ .240

(display-line "s1 is the result of using (solve)")
(display-line "s2 is the expected value (calculated from the solution function)")
(define diffeq-check-points (generate-check-list timestep (list 0.0 0.02 0.05 0.07 0.10 0.13 0.16 0.18 0.20 0.22 0.23)))
(show-streams-at-points  (solve f2 1.0 timestep) 
                         (stream-map  fsol (scale-stream (cons-stream 0 integers) timestep))
                         diffeq-check-points
                         )

; Ex 3.77.
; Alternate method for integral

; Non-delayed:
(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand) 
                   the-empty-stream 
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand)) initial-value)
                             dt
                             )
                   )
               )
  )


; Verifying
(newline)
(display-line "Checking integral method")
(display-line "Integral of polynomial")
(display-line "s1 is the result of integral")
(display-line "s2 is the expected value (calculated from the solution function)")
(show-streams-at-points (integral (delay (stream-map f1 (point-stream integral-step))) 0 integral-step)
                        (stream-map int-f1 (point-stream integral-step))
                        integral-check-points
                        )

(newline)
(display-line "Checking solve using new integral method (calculating e)")
(display "Known value of e to 12 decimal places: ")
(display euler-num)
(newline)
(display "Calculated value: ")
(define calced-e-revised (stream-ref (solve (lambda (y) y) 1.0 timestep) (to-index (/ 1.0 timestep)) ) )
(display calced-e)
(newline)
(display "Difference is ")
(display (abs (- euler-num calced-e-revised)))
(newline)


(newline)
(display-line "Checking solve (with new integral method) using another equation")
(display-line "s1 is the result of using (solve)")
(display-line "s2 is the expected value (calculated from the solution function)")
(show-streams-at-points (solve f2 1.0 timestep) 
                        (stream-map  fsol (scale-stream (cons-stream 0 integers) timestep))
                        diffeq-check-points
                        )

; Ex 3.78.
; Differential solver

; << Define 2nd-order solver >>

; Testing solver
(newline)
(display-line "Testing 2nd-order equation solver")
(set! timestep 0.01)

(define test-2nd-order << Call solver with a=4 b=-4 dt=timestep y0=1 dy0=2 >>)  ; y'' - 4y' + 4y = 0 -> y = (e^(2t))

(define fsolution-t2o (stream-map  (lambda (t) (expt euler-num (* 2 t))) (scale-stream (cons-stream 0 integers) timestep)))

(define test-2nd-order-points (generate-check-list timestep (list 0 2 5 10 20 30 50 80 100 120 167 213 280)))

(display-line "s1 is the result of the solver")
(display-line "s2 is the expected result (from solution function)")

(show-streams-at-points test-2nd-order
                        fsolution-t2o
                        test-2nd-order-points
                        )

; Calculate cumulative error
(display "Amount of relative error at t=3.0 : ")
(display (abs (- 1 (/ (stream-ref test-2nd-order (to-index (/ 3.0 timestep))) (* (expt euler-num 6))))))
(newline)

; Ex 3.79.
; General 2nd-order solver

; << define general 2nd-order solver >>

; Testing general 2nd-order solver
(newline)
(display-line "Solving 2nd-order differential equations (generic approach)")

; Same function as before (in 3.78)
(define (lin2nd dy y)
  (+ (* dy 4) (* y -4))
  )

(define test-generic-2nd-order (<< Call solver with f=lin2nd dt=timestep y0=1 dy0=2 >>))

(display-line "s1 is the result of the solver")
(display-line "s2 is expected (from solution function)")
(show-streams-at-points test-generic-2nd-order
                           fsolution-t2o
                           test-2nd-order-points
                           )

(display "Amount of relative error at t=3.0 : ")
(display (abs (- 1 (/ (stream-ref test-generic-2nd-order (to-index (/ 3.0 timestep))) (* (expt euler-num 6))))) )
(newline)

; Non-linear function: Duffing equation with damping, no forcing
; (models an oscillator with two stable positions, a 'double well')
; This does not have an easily-expressed exact solution, but the damping
; will lead to a final state in one of the two 'wells', given enough time.

; Generates a damped equation function, with gamma as the damping coefficient
(define (make-damped-duffing gamma)
  (lambda (dy y)
    (+ (- (* gamma dy))
     y
     (- (expt y 3))
     )
    )
  )

(newline)
(display-line "Examining solution of non-linear equations")

(define initial-vel 4.0)

(define osc1 << Call solver with f=(make-damped-duffing 0.06) dt=timestep y0=1 dy0=initial-vel>>)
(define osc2 << Call solver with f=(make-damped-duffing 0.20) dt=timestep y0=1 dy0=initial-vel>>)
  
(define oscillator-test-points (generate-check-list timestep '(0 1 2 3 4 5 10 20 50 100 101 102 103 104 105 110 500)))

(display-line "s1 and s2 have different conditions, but approach the same (absolute) values")
(show-streams-at-points osc1 osc2 oscillator-test-points)
; Observation should show initial oscillation followed by settling near either -1.0 or +1.0
; Tweaking initial-vel, damping coefficient will affect the time taken. Modifying initial position most
; likely will not change the final outcome (assuming initial 'velocity' [dy0] is high enough).
; Note that adjusting the timestep could lead to incorrect output.

  
; Ex 3.80.
; RLC circuit solver

; << Define RLC >>

; Testing
(newline)
(display-line "Testing RLC")
; R = 1 ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second
(define test-circuit << Call RLC R=1.0 L=1.0 C=0.2 dt=0.1 >>)

(display-line "All circuits have Il0 = 0A, Vc0 = 10V.")
(display-line "Initial example")
; Initial values iL0 = 0, vC0 = 10V 
(define results (test-circuit 10 0))
(display "Voltage: ")
(display-n-of-stream 40 (stream-reduce (car results) 10))
(display "Current: ")
(display-n-of-stream 40 (stream-reduce (cdr results) 10))

(newline)
(display-line "Checking variation in circuit values")

(define test-circuit-ud  << Call RLC R=1.0 L=0.5 C=0.2 dt=0.1 >>)  ; underdamped

(define test-circuit-cd << Call RLC R=2.0 L=0.5 C=0.5 dt=0.1 >>) ; critically damped

; overdamped
(define test-circuit-expl << Call RLC R=10.0 L=0.2 C=1.0 dt=0.1 >>)  ; what should happen vs what does 
(define test-circuit-fix << Call RLC R=10.0 L=0.2 C=1.0 dt=0.01 >>) ; (how to fix it)

(define results-under (test-circuit-ud 10 0))
(define results-crit (test-circuit-cd 10 0))
(define results-over (test-circuit-expl 10 0))
(define results-fix-over (test-circuit-fix 10 0))

(display-line "An underdamped system (similar to original example)")
(display "Voltage: ")
(display-n-of-stream 20 (stream-reduce (car results-under) 10))
(display "Current: ")
(display-n-of-stream 20 (stream-reduce (cdr results-under) 10))

(display-line "A critically damped system")
(display "Voltage: ")
(display-n-of-stream 20 (stream-reduce (car results-crit) 10))
(display "Current: ")
(display-n-of-stream 20 (stream-reduce (cdr results-crit) 10))


(display-line "A heavily overdamped system (with problems)")
(display "Voltage: ")         
(display-n-of-stream 20 (stream-reduce (car results-over) 10))
(display "Current: ")
(display-n-of-stream 20 (stream-reduce (car results-over) 10))

(display-line "A heavily overdamped system (problem fixed)")
(display "Voltage: ")
(display-n-of-stream 20 (stream-reduce (car results-fix-over) 100))
(display "Current: ")
(display-n-of-stream 20 (stream-reduce (cdr results-fix-over) 100))

; Optional for Racket (generates plots of RLC results)
;(display-line "Plotted values")
;(require plot)
;(define rlc-x (get-n-of-stream 100 (point-stream 0.1)) )
;(define cd-v1 (get-n-of-stream 100 (car results-crit)) )  
;(define cd-i1 (get-n-of-stream 100 (cdr results-crit)) )

;(display-line "Critically damped circuit")
;(plot (points (map vector rlc-x cd-v1 )) )
;(plot (points (map vector rlc-x cd-i1 )) )

;(define ud-v1 (get-n-of-stream 100 (car results-under)))
;(define ud-i1 (get-n-of-stream 100 (cdr results-under)))

;(display-line "Underdamped circuit")
;(plot (points (map vector rlc-x ud-v1)) )
;(plot (points (map vector rlc-x ud-i1)) )

;;The higher reduce value here is not due to the timestep difference, but because the
;; overdamped system takes far longer to change.
;(define rlc-x-dt (get-n-of-stream 100 (stream-reduce (point-stream 0.01) 100)) )
;(define od-v1 (get-n-of-stream 100 (stream-reduce (car results-fix-over) 100)) )
;(define od-i1 (get-n-of-stream 100 (stream-reduce (cdr results-fix-over) 100)) )

;(display-line "Heavily overdamped circuit (note time scale)")
;(plot (points (map vector rlc-x-dt od-v1)) )
;(plot (points (map vector rlc-x-dt od-i1)) )


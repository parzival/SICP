; Section 3.3.5

; For R5RS in DrRacket only
;(#%require srfi/64)
;(#%require (only racket/base
;                 error
;                 )
;           )

; For other Scheme interpreters, this may work - default implementation of SRFI-64
;(load "library/srfi64-testing.scm")

; Chicken Scheme only (some Schemes may have similar methods)
; Chicken has srfi-64 as an egg, use
; chicken-install srfi-64  
; at command line to install, then use one of the following lines here:
; (require-extension srfi-64) ; Chicken 4
; (import srfi-64)            ; Chicken 5

; Define as necessary
(define false #f)
(define true #t)

(define (displayln s)
  (display s)
  (newline)
  )

; Variables used for testing

(define test-epsilon 1.0e-6)
(define g-silence-probes false) ; Enables/disables probes globally
(define t-quiet-probes true) ; Enables/disables probes when running tests

(define (run-tests test-procedure)
  (let (( silence-prev g-silence-probes)
       )
       (if t-quiet-probes (set! g-silence-probes true))
       (test-runner-current (test-runner-create))  ; Makes a new (default) test-runner
       (test-procedure)
       (set! g-silence-probes silence-prev)
       )
  )

; Additional helper/test functions and variables
(define (test-connector-= con val)
  (if (has-value? con)
      (test-approximate (get-value con) val test-epsilon)
      (test-assert "Connector has no value" #f) ; Automatic fail
      )
  )


; helper functions

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items))
           )
          (else (procedure (car items))
                (loop (cdr items))
                )
          )
    )
  (loop list)
  )
; Connectors

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints
                              )
             )
            ((not (= value newval))
             (error "Contradiction" (list value newval))
             )
            (else 'ignored)
            )
      )
    
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant false)
            (for-each-except retractor
                             inform-about-no-value
                             constraints
                             )
            )
          'ignored
          )
      )
    
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints))
          )
      (if (has-value? me)
          (inform-about-value new-constraint)
          )
      'done
      )
    
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false)
             )
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))
            )
      )
    
    me
    )
  )

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant)
  )
(define (forget-value! connector retractor)
  ((connector 'forget) retractor)
  )
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint)
  )


; Constraint information passing

(define (inform-about-value constraint) (constraint 'I-have-a-value))
(define (inform-about-no-value constraint) (constraint 'I-lost-my-value))


; Simple constraints

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request)
    )
  (connect connector me)
  (set-value! connector value me)
  me
  )

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me
                       )
           )
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me
                       )
           )
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me
                       )
           )
          )
    )
  
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value)
    )
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value)
           )
          ((eq? request 'I-lost-my-value)
           (process-forget-value)
           )
          (else
           (error "Unknown request -- ADDER" request)
           )
          )
    )
  
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me
  )

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0))
               )
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me
                       )
           )
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me
                       )
           )
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me
                       )
           )
          )
    )
  
  (define (process-forget-value)
    (forget-value! product me) (forget-value! m1 me) (forget-value! m2 me) (process-new-value))
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value)
           )
          ((eq? request 'I-lost-my-value)
           (process-forget-value)
           )
          (else
           (error "Unknown request -- MULTIPLIER" request))
          )
    )
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me
  )

(define (probe name connector)
  (define (print-probe value)
    (if (not g-silence-probes)
      (begin
        (newline)
        (display "Probe: ")
        (display name)
        (display " = ")
        (display value)
        )
      )
    )
  
  (define (process-new-value)
    (print-probe (get-value connector))
    )
  
  (define (process-forget-value) 
    (print-probe "?")
    )
  
  (define (me request)
    (cond 
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else
       (error "Unknown request -- PROBE" request)
       )
      )
    )
  (connect connector me)
  me
  )

(define (forget-all conlist retractor)
  (for-each (lambda (c) (forget-value! c retractor)) conlist)
  )

; Verify working constraint system

(define (celsius-fahrenheit-converter c f) 
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector))
        )
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok
    )
  )

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F)

(displayln "Verifying temperature converter")

; Add probes
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

; Modify some values
(set-value! C 25 'user)

;(set-value! F 212 'user) ; Error
(forget-value! C 'user)

(set-value! F 212 'user)

(forget-all (list C F) 'user)
(newline)

; Using the test framework (SRFI-64)

(define (temp-converter-tests)
  (define (clear-all)
    (forget-all (list Ct Ft) 'test)
    )
  (test-group
  "Temperature converter tests"
  (test-group
   "Basic connection tests"
   (test-group-with-cleanup
    "C can be set"
    (begin
      (set-value! Ct 1 'test)
      (test-assert (has-value? Ct))
      (test-assert (has-value? Ft))
      (test-connector-= Ct 1)
      )
    (clear-all) ; Clean-up
    )
   (test-group-with-cleanup
    "F can be set"
    (begin
      (set-value! Ft 25 'test)
      (test-assert (has-value? Ft))
      (test-assert (has-value? Ct))
      (test-connector-= Ft 25)
      )
    (clear-all) ; Clean-up
    )
   ) ; End Basic connection tests
  (test-group
   "Calculation tests"
   (test-group-with-cleanup
    "Setting C sets F correctly"
    (begin
      (set-value! Ct 0 'test)
      (test-connector-= Ft 32)
      )
    (clear-all) ; Clean-up
    )
   (test-group-with-cleanup
    "Setting F sets C correctly"
    (begin
      (set-value! Ft 50 'test)
      (test-connector-= Ct 10)
      )
    (clear-all) ; Clean-up
    )
    (test-group-with-cleanup
    "Non-integer values computed correctly (C->F)"
    (begin
      (set-value! Ct 37 'test)
      (test-connector-= Ft 98.6)
      )
    (clear-all) ; Clean-up
    )
   ) ; End Calculation tests
   ) ; End Temperature Converter tests
  )
(define Ct (make-connector))
(define Ft (make-connector))

(celsius-fahrenheit-converter Ct Ft)
(run-tests temp-converter-tests)

; Ex. 3.33.
; Averager constraint

(define (averager a b c)
  ; << c should be average of a & b >>
   'nope
  )

; Testing
(displayln "Testing averager")


(define (averager-tests)
  (let ((a1 (make-connector))
        (b1 (make-connector))
        (c1 (make-connector))
        )
    (define (clear-all)
      (forget-all (list a1 b1 c1) 'test)
      )

    (averager a1 b1 c1)
    (test-group
     "Averager tests"
     (test-group
      "Basic connection tests"
      (test-group-with-cleanup
       "Setting a1 works"
       (begin
         (set-value! a1 6 'test)
         (test-assert (has-value? a1))
         (test-assert (not (has-value? b1)))
         (test-assert (not (has-value? c1)))
         (test-connector-= a1 6)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "Setting b1 works"
       (begin
         (set-value! b1 10 'test)
         (test-assert (has-value? b1))
         (test-assert (not (has-value? a1)))
         (test-assert (not (has-value? c1)))
         (test-connector-= b1 10)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "Setting c1 works"
       (begin
         (set-value! c1 7 'test)
         (test-assert (has-value? c1))
         (test-assert (not (has-value? a1)))
         (test-assert (not (has-value? b1)))
         (test-connector-= c1 7)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "Setting a1 and b1 sets c1"
       (begin
         (set-value! a1 5 'test)
         (set-value! b1 10 'test)
         (test-assert (has-value? c1))
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "Setting a1 and c1 sets b1"
       (begin
         (set-value! a1 5 'test)
         (set-value! c1 7 'test)
         (test-assert (has-value? b1))
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "Setting b1 and c1 sets a1"
       (set-value! b1 10 'test)
       (set-value! c1 8 'test)
       (test-assert (has-value? a1))
       )
      (clear-all)
      )
     (test-group
      "Calculation tests"
      (test-group-with-cleanup
       "All values equal if any two equal (a1 and b1 set)"
       (begin
         (set-value! a1 1 'test)
         (set-value! b1 1 'test)
         (test-connector-= c1 1)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "c1 is average of a1 and b1"
       (begin
         (set-value! a1 5 'test)
         (set-value! b1 11 'test)
         (test-connector-= c1 8)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "a1 correct when b1 and c1 set"
       (begin
         (set-value! b1 -3 'test)
         (set-value! c1 -2 'test)
         (test-connector-= a1 -1)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "b1 correct when a1 and c1 set"
       (begin
         (set-value! a1 -6 'test)
         (set-value! c1 1 'test)
         (test-connector-= b1 8)
         )
       (clear-all)
       )
      (test-group-with-cleanup
       "Non-integer values computed correctly"
       (begin
         (set-value! a1 5 'test)
         (set-value! b1 10 'test)
         (test-connector-= c1 7.5)
         )
       (clear-all)
       )
      )
     )
    )
  )

(run-tests averager-tests)


; Ex 3.34.
; Explain the flaw in this version of squarer

(define (squarer a b)
  (multiplier a a b)
  )

; Tests for squarer
(define (squarer-tests)
  (let ((as (make-connector))
        (bs (make-connector))
        )
    (define (clear-all)
      (forget-all (list as bs) 'test)
      )
    (squarer as bs)
    (test-group
     "Squarer tests"
     ; Example test:
        (test-group-with-cleanup
          "a sets b"
          (begin
            (set-value! as 4 'test)
            (test-assert (has-value? as))
            (test-assert (has-value? bs))
            (test-connector-= as 4)
            )
          (clear-all)
          )
          ; << Add additional groups/tests >>
          ; examples:
          ; "b sets a"
          ; "Forgetting b clears a when set"
          ; "Forgetting a clears b when set"
          ; "b = a squared"
          ; "a is square root of b"
          ; 
          ; Test format:
         ;(test-group-with-cleanup
          ; << test title >>
          ;(begin
            ; << test statements sequence >>
          ;  )
          ;(clear-all)
          ;)
        ); End of Squarer test group
     )
  )

(displayln "Testing FAULTY Squarer ")

(run-tests squarer-tests)

; Ex 3.35.
;
;(define (squarer a b)
;  (define (process-new-value)
;    (if (has-value? b)
;        (if (< (get-value b) 0)
;            (error "square less than 0 -- SQUARER" (get-value b))
;            <?alternative1?>)
;        <alternative2>
;        )
;    )
;  
;  (define (process-forget-value)
;    <<body1>>
;    )
;  
;  (define (me request)
;    <<body2>>
;    )
;  
;  <<rest of definition>>
;  me
;  )


; Testing
(displayln "Testing squarer (primitive)")

(run-tests squarer-tests)


; Ex 3.36. Environment diagram when for-each-except is executed

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)


; Ex 3.37.

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x
          )
      (cv 32)
      )
  )


(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z
    )
  )

; << define c-, c*, c/, cv >>

; Testing
(displayln "Testing Expression-oriented temperature converter")

; Redefine these using our new version
(set! Ct (make-connector))
(set! Ft (celsius-fahrenheit-converter Ct))

; Run the same set of tests
(run-tests temp-converter-tests)
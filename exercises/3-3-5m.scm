; Section 3.3.5

; This file's tests use the Test-manager format, usable for MIT Scheme only

; Test-manager can be found at : https://github.com/axch/test-manager.git
; If you have git installed, go to your own library/mit-scheme directory, and
; execute
; >git clone 'https://github.com/axch/test-manager.git'
; which will put a copy of the test-manager project in that directory. 
; You can also just download the project entirely (as a zip file) and extract it to
; that directory, if you do not use git. 
; Only one file needs to be loaded here, but the test-manager directory needs to have
; the full project in it to work.
; This version appears to give warnings on every test (using MIT Scheme 11), but they
; can be ignored.
(load "library/mit-scheme/test-manager/load.scm")

(define (displayln s)
  (display s)
  (newline)
  )

; Variables used for testing

(define test-epsilon 1.0e-6)
(define g-silence-probes false) ; Enables/disables probes globally
(define t-quiet-probes true) ; Enables/disables probes when running tests

(define (run-tests test-group)
  (let (( silence-prev g-silence-probes)
       ) 
       (if t-quiet-probes (set! g-silence-probes true))
       (run-test (list test-group))
       (set! g-silence-probes silence-prev)
       )
  )

; Additional helper/test functions and variables
(define (test-connector-= con val)
  (if (has-value? con)
      (assert-in-delta val (get-value con) test-epsilon)
      (assert false  "Connector has no value") ; Automatic fail
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

; Using the test framework 
(define (tc-clear-all)
    (forget-all (list Ct Ft) 'test)
  )
  
(in-test-group
    t-temp-converter
   (in-test-group
     t-basic-connection
     (define-tear-down (tc-clear-all))
     (define-test (csets)
     "C can be set"
        (set-value! Ct 1 'test)
        (check (has-value? Ct))
        (check (has-value? Ft))
        (test-connector-= Ct 1)
        )
     (define-test (fsets)
      "F can be set"
        (set-value! Ft 25 'test)
        (check (has-value? Ft))
        (check (has-value? Ct))
        (test-connector-= Ft 25)
        )
      ) ; End Basic connection tests
    (in-test-group
     t-calculation
     (in-test-group
      t-setting-one-sets-other
      (define-tear-down (tc-clear-all))
      (define-test (csetsf)
      "Setting C sets F correctly"
        (set-value! Ct 0 'test)
        (test-connector-= Ft 32)
        )
      (define-test (fsetsc)
      "Setting F sets C correctly"
        (set-value! Ft 50 'test)
        (test-connector-= Ct 10)
        )
      )
     (in-test-group
      non-integer-values
      (define-test (fractional-ctof)
      "Non-integer values computed correctly (C->F)"
        (set-value! Ct 37 'test)
        (test-connector-= Ft 98.6)
        )
      )
    ) ; End Calculation tests
  ) ; End Temperature Converter tests


(define temp-converter-tests 't-temp-converter)

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

(define (at-clear-all)
  (forget-all (list a1 b1 c1) 'test)
  )
  
(define a1 (make-connector))
(define b1 (make-connector))
(define c1 (make-connector))
  
(in-test-group 
  t-averager
     (in-test-group
      t-basic-connection
      (define-tear-down (at-clear-all))
      (define-test (a1only)
        "Setting a1 works"
        (set-value! a1 6 'test)
        (check (has-value? a1))
        (check (not (has-value? b1)))
        (check (not (has-value? c1)))
        (test-connector-= a1 6)
        )
      (define-test (b1-only)
       "Setting b1 works"
        (set-value! b1 10 'test)
        (check (has-value? b1))
        (check (not (has-value? a1)))
        (check (not (has-value? c1)))
        (test-connector-= b1 10)
        )
      (define-test (c1-only)
       "Setting c1 works"
        (set-value! c1 7 'test)
        (check (has-value? c1))
        (check (not (has-value? a1)))
        (check (not (has-value? b1)))
        (test-connector-= c1 7)
        )
      (define-test (a1-b1)
        "Setting a1 and b1 sets c1"
        (set-value! a1 5 'test)
        (set-value! b1 10 'test)
        (check (has-value? c1))
        )
      (define-test (a1-c1)
        "Setting a1 and c1 sets b1"
        (set-value! a1 5 'test)
        (set-value! c1 7 'test)
        (check (has-value? b1))
        )
      (define-test (b1-c1)
        "Setting b1 and c1 sets a1"
        (set-value! b1 10 'test)
        (set-value! c1 8 'test)
        (check (has-value? a1))
        )
     (in-test-group
      t-calculation
      (define-tear-down (at-clear-all))
      (define-test (equal-from-two)
        "All values equal if any two equal (a1 and b1 set)"
        (set-value! a1 1 'test)
        (set-value! b1 1 'test)
        (test-connector-= c1 1)
        )
      (define-test (c-averaging)
        "c1 is average of a1 and b1"
        (set-value! a1 5 'test)
        (set-value! b1 11 'test)
        (test-connector-= c1 8)
        )
      (define-test (a-set-from-others)
        "a1 is correct when b1 and c1 set"
        (set-value! b1 -3 'test)
        (set-value! c1 -2 'test)
        (test-connector-= a1 -1)
        )
      (define-test (b-set-from-others)
        "b1 is correct when a1 and c1 set"
        (set-value! a1 -6 'test)
        (set-value! c1 1 'test)
        (test-connector-= b1 8)
        )
      (define-test (non-integers)
        "Non-integer values computed correctly"
        (set-value! a1 5 'test)
        (set-value! b1 10 'test)
        (test-connector-= c1 7.5)
        )
      )
    )
  )
(define averager-tests 't-averager)

(averager a1 b1 c1)

(run-tests averager-tests)


; Ex 3.34.
; Explain the flaw in this version of squarer

(define (squarer a b)
  (multiplier a a b)
  )

; Tests for squarer
(define (sq-clear-all)
  (forget-all (list as bs) 'test)
  )
 
; Need an initial define, these connectors are not used 
(define as (make-connector))
(define bs (make-connector))
;(squarer as bs) ; Not needed, as a new squarer is generated each run


(in-test-group
  t-squarer
  (define-group-set-up (lambda ()
                 (set! as (make-connector))
                 (set! bs (make-connector))
                 (squarer as bs)
                 )
  )
   (in-test-group
     t-basic-connection
     (define-tear-down (sq-clear-all))
     ; Example test:
     (define-test (asetsb)
       "a sets b"
       (set-value! as 4 'test)
       (check (has-value? as))
       (check (has-value? bs))
       (test-connector-= as 4)
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
        ;(define-test (<<test-label>>)
        ; "<< test title >>"
        ; << test statements sequence >>
        ;)
     ); End of 'basic' test group
  )

(define squarer-tests 't-squarer)

(displayln "Testing FAULTY Squarer ")

(run-tests squarer-tests)
(newline)

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

(define as (make-connector))
(define bs (make-connector))
(squarer as bs)

(run-tests squarer-tests)
(newline)


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
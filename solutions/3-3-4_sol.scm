; Section 3.3.4

(load "library/queuedef.scm")

(define (displayln line)
  (display line)
  (newline)
  )

(define (logical-not s)
  (cond ((= s 0) 1) 
        ((= s 1) 0)
        (else (error "Invalid signal" s))
        )
  )

; included for help

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ; Output 0 as long as any input is 0
        ((= s1 0) 0)
        ((= s2 0) 0)    
        (else (error "Invalid signals in (logical-and)"))
        )
  )

(define (logical-or s1 s2)
  (cond ; Output 1 as long as any input is 1
        ((= s1 1) 1)
        ((= s2 1) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signals in (logical-or)"))
        )
  )

; Supplied function boxes (inverter and and-gate)
(define (inverter input output) 
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value))
                   )
      )
    )
  
  (add-action! input invert-input)
  'ok
  )

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda ()
                                    (set-signal! output new-value)
                                    )
                   )
      )
    )
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok
  )

; Adder components
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire))
        )
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok
    )
  )

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire))
        )
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok
    )
  )


; Constructor for wires
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures)
            'done
            )
          )
      )
    
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)
      )
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))
            )
      )
    
    dispatch
    )
  )

(define (call-each procedures) 
  (if (null? procedures)
      'done 
      (begin
        ((car procedures))
        (call-each (cdr procedures))
        )
      )
  )

(define (get-signal wire) (wire 'get-signal))

(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

; Agenda operations
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda
                  )
  )

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate)
        )
      )
  )

(define (probe name wire) 
  (add-action! wire
               (lambda () 
                 ;(newline)
                 (display name)
                 (display " ") 
                 (display (current-time the-agenda)) 
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline)
                 )
               )
  )


; Agenda implementation

; Time segments
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s)) 
(define (segment-queue s) (cdr s))

; Agendas
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time)
  )
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments)
  )
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments) (< time (segment-time (car segments))))
    )
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q))
    ) 
  
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action
                       )
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                         (cdr segments)
                                         )
                         )
              (add-to-segments! rest)
              )
          )
        )
    )
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action)
                                     segments
                                     )
                       )
        (add-to-segments! segments)
        )
    )
  )

(define (remove-first-agenda-item! agenda) 
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
        )
    )
  )

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM") 
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg))
        )
      )
  )


; Ex 3.28.
; OR-gate

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda ()
                                   (set-signal! output new-value)
                                   )
                   )
      )
    )
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok
  )

; Ex 3.29.
; OR-gate built from NOT and AND gates

(define (or-from-gates a b output)
  (let ((ia (make-wire))
        (ib (make-wire))
        (and-out (make-wire))
        )
    (inverter a ia)
    (inverter b ib)
    ;(set-signal! ia 1) ; initial value 
    ;(set-signal! ib 1) ; initial value
    (and-gate ia ib and-out)
    ;(set-signal! and-out 1) ; initial value - needed if the initial state is assumed 0
    (inverter and-out output)
    'ok
    )
  )

; Delay is two inverter-delays plus an and-gate-delay


; Ex 3.30.
; Ripple adder

(define (ripple-carry-adder Ak Bk Sk C)
  ; set up carry-in list, MSB first 
  (let ((Cins (map (lambda(x) (make-wire)) Ak))
        )
    ; set up carry-outs
    (let ((Couts (cons C (reverse (cdr (reverse Cins)))))
          )
        (set-signal! (car (reverse Cins)) 0) ; Tie lowest carry-in to 0
        (for-each full-adder Ak Bk Cins Sk Couts)
      )
    )
  'ok
  )


; Ex 3.31.


; ans: half-adder does not update until some critical part of the internal state changes; it is not properly initialized.


; Verification (example from text)

(displayln "Verifying example with half-adder")
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(displayln "*Setting input-1 to 1*")
(set-signal! input-1 1)

(displayln "Running...")
(propagate)

(newline)
(displayln "*Setting input-2 to 1*")
(set-signal! input-2 1)

(displayln "Running...")
(propagate)
(newline)


; More detailed tests

(define (test-signal wire expected-value testing-time . name)
  (define (test-action)
    (let ((actual-value (get-signal wire))
          )
      (if (not (null? name))
          (display (car name))
          (display "test")
          )
      (display ": ")
      (if (= expected-value actual-value)
          (display "passed")
          (begin
            (display "test failed: Expected ")
            (display expected-value)
            (display " but was ")
            (display actual-value)
            )
          )
      (newline)
      )
    )
  
  (add-to-agenda! testing-time
                  test-action
                  the-agenda
                  )
  'test-added
  )

(define (set-signal-at-time! wire new-value set-time)
  (add-to-agenda! set-time
                  (lambda () (set-signal! wire new-value))
                  the-agenda
                  )
  'signal-to-set
  )

(define (show-message-at-time! msg set-time)
  (add-to-agenda! set-time
                  (lambda () (displayln msg))
                  the-agenda
                  )
  )

; Testing

; Set up wire tests for gates

(displayln "Running tests using test-signal")

; Input wires for testing
(define i1 (make-wire))
(define i2 (make-wire))

; Times at which to run each test in sequence
(define test-time1 0)
(define test-time2 10)
(define test-time3 20)

(displayln "Tests for INVERTER")

(set-current-time! the-agenda 0)

(define i1 (make-wire))
(define io1 (make-wire))
(inverter i1 io1)

(test-signal io1 0 0 "initial value")
(test-signal io1 1 (+ 1 test-time1 inverter-delay) "input low")
(test-signal io1 0 (+ 1 test-time2 inverter-delay) "input high")
(test-signal io1 1 (+ 1 test-time3 inverter-delay) "input switches low")
(set-signal-at-time! i1 0 test-time1)
(set-signal-at-time! i1 1 test-time2)
(set-signal-at-time! i1 0 test-time3)

(displayln "Running INVERTER tests")
(propagate)

; Tests for the AND gate
(newline)
(displayln "Tests for AND gate")
(set-current-time! the-agenda 0)

(define ao1 (make-wire))
(and-gate i1 i2 ao1)

(test-signal ao1 0 0 "initial value")
(set-signal-at-time! i2 1 test-time1)
(test-signal ao1 0 (+ 1 test-time1 and-gate-delay) "only input 2 high")
(set-signal-at-time! i1 1 test-time2)
(test-signal ao1 1 (+ 1 test-time2 and-gate-delay) "both set high")
(test-signal ao1 0 (+ 1 test-time3 and-gate-delay) "input 2 switches low") 

(set-signal-at-time! i2 0 test-time3)

(displayln "Running AND tests")
(propagate)

; Tests for the OR gates 
(newline)
(displayln "Tests for OR gate")
(set-current-time! the-agenda 0)

(define oo1 (make-wire))
(or-gate i1 i2 oo1)
(test-signal oo1 0 0 "initial value")
(test-signal oo1 1 (+ 1 test-time1 or-gate-delay) "input 2 high")
(test-signal oo1 1 (+ 1 test-time2 or-gate-delay) "both set high")
(test-signal oo1 1 (+ 1 test-time3 or-gate-delay) "input 2 switches low") 
(set-signal-at-time! i2 1 test-time1)
(set-signal-at-time! i1 1 test-time2)
(set-signal-at-time! i2 0 test-time3)
(displayln "Running OR tests")
(propagate)

(newline)
(displayln "Tests for OR gate made from other gates")
(set-current-time! the-agenda 0)

(define oo2 (make-wire))
(or-from-gates i1 i2 oo2)
(test-signal oo2 0 0 "initial value")
(test-signal oo2 1 (+ 1 test-time1 or-gate-delay) "input 2 high")
(test-signal oo2 1 (+ 1 test-time2 or-gate-delay) "both set high")
(test-signal oo2 1 (+ 1 test-time3 or-gate-delay) "input 2 switches low") 
(set-signal-at-time! i2 1 test-time1)
(set-signal-at-time! i1 1 test-time2)
(set-signal-at-time! i2 0 test-time3)
(displayln "Running OR from other gate tests")
(propagate)

; Set up testing on a set of signals

(define (setup-test-series signals-to-set signals-to-test set-expected-sequence test-interval)
  
  (define (setup-inputs values test-start-time)
    (for-each (lambda (sig val) 
                (set-signal-at-time! sig val test-start-time)
                )
              signals-to-set
              values
              )
    )
  
  (define (setup-tests expected-values test-measure-time)
    (for-each (lambda (sig exp-val)
                (test-signal sig exp-val test-measure-time)
                )
                signals-to-test
                expected-values
                )
    )
  
  (define (test-iter test-time setvals expvals label-list)
    (if (null? setvals)
        'test-setup-done
        (let ((test-end-time (+ test-interval test-time))
              )
          (if (not (null? label-list))
              (show-message-at-time! (car label-list) test-time)
              )
          (setup-inputs (car setvals) test-time)
          (setup-tests  (car expvals) test-end-time)
          (test-iter test-end-time
                     (cdr setvals)
                     (cdr expvals)
                     (if (null? label-list) label-list (cdr label-list))
                     )  
          )
        )
    )

  (define (extract-labels current-labels se-list)
    (if (null? se-list)
        current-labels
        (extract-labels
         (if (null? (cddr (car se-list)))
             current-labels
             (append current-labels (cddr (car se-list)))
             )
         (cdr se-list)
         )
        )
    )
  
  (test-iter 0
             (map (lambda (se-pair) (car se-pair)) set-expected-sequence)
             (map (lambda (se-pair) (cadr se-pair)) set-expected-sequence)
             (extract-labels '() set-expected-sequence)
             )
  )


(define i1 (make-wire))
(define i2 (make-wire))
(define i3 (make-wire))

; Test Half-adder to demonstrate test-series
(displayln "Testing HALF-ADDER")
(set-current-time! the-agenda 0)
(define ha-sum (make-wire))
(define ha-car (make-wire))
(half-adder i1 i2 ha-sum ha-car) 

(setup-test-series (list i1 i2)                      ; the input wires
                   (list ha-sum ha-car)              ; wirest to test
                   (list ; i1 i2   s c
                    (list '(0 0) '(0 0))
                    (list '(0 1) '(1 0))
                    (list '(1 1) '(0 1))
                    (list '(0 1) '(1 0))
                    (list '(1 0) '(1 0))
                    )  
                   15                                ; time between each test
                   )


(propagate)


; Test Ripple Carry Adder 
(displayln "Testing Ripple Adder")
(set-current-time! the-agenda 0)
(define ra-inputA (list (make-wire) (make-wire) (make-wire)))  ; input A (list)
(define ra-inputB (list (make-wire) (make-wire) (make-wire)))  ; input B (list) 
(define ra-Sum    (list (make-wire) (make-wire) (make-wire)))  ; output S (list)
(define ra-cout    (make-wire))                                ; output C

(ripple-carry-adder ra-inputA ra-inputB ra-Sum ra-cout)

; Values appear here with MSB on left.
(define test-inputs (append ra-inputA ra-inputB))

(define inputs-results (list
                        (list '(0 0 0  0 0 0)  '(0 0 0 0) "0 + 0")   ; 0 + 0      = 0
                        (list '(0 0 1  0 0 1)  '(0 0 1 0) "1 + 1")   ; 1 + 1      = 2
                        (list '(0 0 1  1 0 0)  '(0 1 0 1) "1 + 4")   ; 1 + 4      = 5
                        (list '(1 1 0  0 0 1)  '(0 1 1 1) "6 + 1")   ; 6 + 1      = 7
                        (list '(1 1 1  0 0 0)  '(0 1 1 1) "7 + 0")   ; 0 + 7      = 7
                        (list '(1 0 0  1 0 0)  '(1 0 0 0) "4 + 4")   ; 4 + 4      = 8/0 + (carry set)
                        (list '(1 1 0  0 1 1)  '(1 0 0 1) "6 + 3")   ; 6 + 3      = 9/1 + (carry set)
                        (list '(1 1 1  1 1 1)  '(1 1 1 0) "7 + 7")   ; 7 + 7      = 14/6 + (carry set)
                        (list '(0 0 0  0 0 0)  '(0 0 0 0) "all inputs 1->0")   ; testing propagation delay
                        (list '(1 1 1  1 1 1)  '(1 1 1 0) "all inputs 0->1")   ; 
                        (list '(0 0 0  0 0 1)  '(0 0 0 1) "all but LSB of B to 0")   ;
                        (list '(1 1 1  1 1 0)  '(1 1 0 1) "all but LSB of B to 1")   ;
                        )
  )

(setup-test-series test-inputs
                   (cons ra-cout ra-Sum)
                   inputs-results
                   60  ; How small can this be? 49 by estimation
                   )

(propagate)

(displayln "Ripple adder tests done")


; Ex 3.32.
; Order of execution

; All values are set at the same time step, but still are placed in the agenda in the order they are called.
; (This is not a demonstration of the two approaches, but it shows how there could be problems if LIFO were used).

(set-current-time! the-agenda 0)
(define i1 (make-wire))
(define i2 (make-wire))
(define and-out (make-wire))
(define prim-or-out (make-wire))
(define gate-or-out (make-wire))

(or-from-gates i1 i2 gate-or-out)
(or-gate i1 i2 prim-or-out)
(and-gate i1 i2 and-out)

(newline)
(displayln "Verifying sequential execution")
(probe 'or-output-gates gate-or-out)
(probe 'or-output-prim prim-or-out)
(probe 'and-output and-out)

(newline)
(displayln "i1 = 0, i2 = 1 (i1 set first)")
(set-signal! i1 0)
(set-signal! i2 1)
(propagate)
(newline)
(displayln "i1 = 1, i2 = 0 (i1 set first)")
(set-signal! i1 1)
(set-signal! i2 0)
(propagate)
(newline)
(displayln "i1 = 0, i2 = 0 (i1 set first)")
(set-signal! i1 0)
(set-signal! i2 0)
(propagate)
(newline)
; Now enter them in a different order
(displayln "i1 = 0, i2 = 1 (i2 set first)")
(set-signal! i2 1)
(set-signal! i1 0)
(propagate)
(newline)
(displayln "i1 = 1, i2 = 0 (i2 set first)")
(set-signal! i2 0)
(set-signal! i1 1)
(propagate)

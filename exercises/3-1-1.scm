; Section 3.1.1

; Note that check-eqv requires 'format'. An easy alternative here is to replace the line
; with a sequence of *display* procedures to output the values.

; Basic equality test (halts on errors)
(define (check-eqv? observed expected . label-args)
  (if (eqv? observed expected)
      (display "pass")
      (display (format "FAILURE: ~a is not equal to ~a." observed expected))
      )
  (if (not (null? label-args))
      (begin
        (display "...")
        (display (car label-args))
        )
      )
  (newline)
  )

; Ex. 3.1.
; Accumulator generator

; >>Define make-accumulator 

; Testing
(displayln "Testing (make-accumulator)")

(define A (make-accumulator 5))
(check-eqv? (A 0) 5 "Accumulator maintains initial value")
(check-eqv? (A 10) 15 "Accumulator can increase value") ; Note: This test causes the value to change.
(define B (make-accumulator 0))
(displayln "Changing Accumulator B:")
(B 8)
(B -11)
(check-eqv? (A 0) 15 "Modifying Acc. B does not alter Acc. A")  
(check-eqv? (B 0) -3 "Acc. B has the proper value after checking Acc. A")


; Ex. 3.2.
; Procedure call counter

; >>Define make-monitored

; Testing
(define s (make-monitored sqrt))

(newline)
(displayln "Demonstrating make-monitored (using sqrt function)")
(display "Calls before running: ")
(s 'how-many-calls?)
(display "Result of function call: ")
(s 100)
(display "Call count: ")
(s 'how-many-calls?)
(newline)

(displayln "Testing monitored procedures (sequence of calls using sqrt)")
(check-eqv? (s 'how-many-calls?) 1 "Initial value of call-count is at 1")
(check-eqv? (s 49) 7 "Monitored procedure gives correct results.")
(check-eqv? (s 'how-many-calls?) 2 "Call count increments after first call")
(display "Non-monitored call: ")
(sqrt 16)
(check-eqv? (s 'how-many-calls?) 2 "Running non-monitored procedure does not affect monitored count.")

(displayln "Testing that multiple monitored functions do not interfere")
(define s2 (make-monitored display))
(check-eqv? (s2 'how-many-calls?) 0 "Initial value of call-count is 0")
(s2 "message from monitored display")
(newline)
(check-eqv? (s2 'how-many-calls?) 1 "Call count increments properly for second procedure")
(check-eqv? (s  'how-many-calls?) 2 "Call count for first procedure does not change")

(newline)
(displayln "Testing reset works as intended")
(s  'reset-count) 
(check-eqv? (s 'how-many-calls?) 0 "Reset properly sets count to 0")
(check-eqv? (s2 'how-many-calls?) 1 "Reset does not affect other monitored procedures")
(display "Calling monitored function again: ")
(s 169)
(check-eqv? (s 'how-many-calls?) 1 "Call count can change after reset")
(newline)

(displayln "Testing that special monitoring messages are not sent to the procedure")
(define s3 (make-monitored (lambda (x)
                             (if (or (eq? x 'how-many-calls?) (eq? x 'reset-count))
                                 (error "make-monitored is passing its messages to monitored procedure.")
                                 (display "Monitored function is okay.")
                                 )
                             )
                           )
  )

(display "Calls to a special monitored function:")
(s3 'how-many-calls?)
(s3 'reset-count)
(s3 'anything-not-special)
(check-eqv? (s3 'how-many-calls?) 1 "Call-count only increments on actual procedure calls" )

; What happens?
;(define mon-mult (make-monitored *))
;(mon-mult 'how-many-calls?)
;(mon-mult 5 4)

; Ex. 3.3.
; Password-protected (make-account)

; Version from text >> modify to require password
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)



; Testing

(newline)
(displayln "Demonstrating an account with a password:")
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

(newline)
(displayln "Testing account operation with password")
(check-eqv? ((acc 'secret-password 'deposit) 20) 80 "Deposits can be made with the password")
(check-eqv? ((acc 'secret-password 'withdraw) 20) 60 "Withdrawals can be made with the password")
(check-eqv? ((acc 'some-other-password 'withdraw) 50) 'incorrect-password "Incorrect password returns a complaint") ; This specific return value is not necessarily a requirement
(check-eqv? ((acc 'secret-password 'deposit) 0) 60 "Invalid access does not change the account value") ; Relies on previous tests


; Ex 3.4.
; Limited password attempts

;>> Create a version that fails after multiple password attempts

; The call-the-cops procedure may be redefined.
(define (call-the-cops)
  (displayln "Remain where you are.  The police will arrive shortly to arrest you.")
  )

; Testing
(newline)
(displayln "Demonstrating password police alerts")

(define acc (make-account 1000000 'unguessable-password))

((acc 'password 'withdraw) 1000000)
((acc 'first-name 'withdraw) 1000000)
((acc 'last-name 'withdraw) 1000000)
((acc 'child-name 'withdraw) 1000000)
((acc 'birthday 'deposit) 1000000)
((acc 'hometown 'withdraw) 1000000)
((acc 'sports-team 'deposit) 1000000)
((acc 'correct-horse-battery-staple 'withdraw) 1000000)
((acc 'try-again-before-the-cops-come 'withdraw) 1000000)

((acc 'unguessable-password 'withdraw) 10)

(newline)
(displayln "Testing password alerts")

(define test-acc (make-account 1000 'secret))

; Test function to repeat n failed attempts
(define wrong-password 'bad-password) ; redefine if necessary

(define (fail-attempts n acc)
  (if (> n 0)
      (begin
        ((acc wrong-password 'withdraw) 1000)
        (fail-attempts (- n 1) acc)
        )
      )
  )

; 'Silent alarm', used for testing. 
(define (call-the-cops)
  'cops-have-been-called
  )

;  This shows how to modify a procedure that takes no arguments to be used with make-monitor, which takes one argument.
(define old-ctc call-the-cops)

(define monitored-ctc (make-monitored 
                       (lambda (x) 
                         (old-ctc)
                         )
                       )
  )

(define (call-the-cops)
  (monitored-ctc '() )
  )

; Ensure account continues to function when password attempts are below threshold
(displayln "Testing account operates normally when alarm not triggered")
(fail-attempts 7 test-acc)
(check-eqv? (monitored-ctc 'how-many-calls?) 0)
(check-eqv? ((test-acc 'secret 'deposit) 1000) 2000)

; Require >7 consecutive attempts
(displayln "Testing alarm is not triggered for non-consecutive failed attempts") 
(check-eqv? ((test-acc 'secret 'withdraw) 1) 1999 )
(fail-attempts 4 test-acc)
(check-eqv? (monitored-ctc 'how-many-calls?) 0 "No alert when failed attempts below minimum")
(check-eqv? ((test-acc 'secret 'withdraw) 1) 1998 )
(fail-attempts 4 test-acc)
(check-eqv? (monitored-ctc 'how-many-calls?) 0 "No alert when failed attempts are non-consecutive")
(check-eqv? ((test-acc 'secret 'deposit) 2) 2000)


; Report when failed attempts exceed 7
(displayln "Testing alarm triggered on proper number of attempts")
(fail-attempts 8 test-acc)
(check-eqv? (monitored-ctc 'how-many-calls?) 1)


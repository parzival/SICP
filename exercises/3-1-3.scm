; Section 3.1.3

(load "3-1-1.scm")  ; To get password-protected account
(displayln "********************************")
(displayln "** FINISHED LOADING 3-1-1.scm **")
(displayln "********************************")
(newline)


; Ex 3.7.
; Joint accounts

; >> define make-joint

(displayln "Demonstrating joint accounts")

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'deposit) 50)  ; 150
((paul-acc 'rosebud 'withdraw) 90)      ;  60
((peter-acc 'rosebud 'withdraw) 40)     ; invalid access
((paul-acc 'open-sesame 'deposit) 50)   ; invalid access
((peter-acc 'open-sesame 'deposit) 0)   ;  60

; Testing
(displayln "Testing joint accounts")

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(define mary-acc (make-joint paul-acc 'rosebud 'stewball))
(define bob-acc (make-account 120 'tambourine))

(check-eqv? ((paul-acc 'rosebud 'deposit) 20) 120 "Joint account can deposit to account")
(check-eqv? ((peter-acc 'open-sesame 'deposit) 10) 130 "Original account can still deposit")
(check-eqv? ((paul-acc 'rosebud 'withdraw) 40) 90 "Joint account can withdraw from account")
(check-eqv? ((peter-acc 'open-sesame 'withdraw) 5) 85 "Original account can still withdraw")
(check-eqv? ((paul-acc 'open-sesame 'deposit) 13) "Incorrect password" "Joint account can only use its own password") ; Replace with response for incorrect password
(check-eqv? ((bob-acc 'tambourine 'withdraw) 35) 85 "Joint accounts do not interfere with any other accounts")
(check-eqv? ((mary-acc 'stewball 'deposit) 90) 175 "Chained joint accounts work properly")


; Ex 3.8.
; Evaluation order

; >> Create f, which gives different results depending on eval order

; Testing
(newline)
(displayln "Testing using let statements to alter eval order")

; Right-to-left (= 1)
(define rtl-result
  (let ((a (f 1)))
    (let ((b (f 0)))
      (+ a b)
      )
    )
  )

;(f 0) ; optional - can f handle additional calls in-between the others? 

; Left-to-right (= 0)
(define ltr-result
  (let ((a (f 0)))
    (let ((b (f 1)))
      (+ a b)
      )
    )
  )

(check-eqv? ltr-result 0 "left-to-right should be 0")
(check-eqv? rtl-result 1 "right-to-left should be 1")

; An alternate approach, which depends on the interpreter
(newline)
(displayln "Testing left-to-right vs. right-to-left arguments for function f")

(if (not (= (+ (f 0) (f 1)) (+ (f 1) (f 0))))
    (displayln "pass...Results for f should be different")
    (displayln "FAILED: Results for f should be different")
    )

; Commented values are for an environment that evaluates left-to-right
(+ (f 0) (f 1))  ; 0
(+ (f 1) (f 0))  ; 1
(+ (f 1) (f 0))  ; 1
(+ (f 0) (f 1))  ; 0

(f 1) ; optional: invoke f a single time between checks

(+ (f 1) (f 0))  ; 1
(+ (f 1) (f 0))  ; 1
(+ (f 0) (f 1))  ; 0
(+ (f 0) (f 1))  ; 0

(f 6) ; optional: can f handle different input?

(+ (f 1) (f 0)) ; 0


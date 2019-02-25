; Section 3.2.3


; Ex 3.10
; Different account creation

; Original version
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance
               )
        "Insufficient funds"
        )
    )
  )


; Verifying
(display "Original make-withdraw")
(newline)
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
(W2 40)
(W1 0)

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance
                 )
          "Insufficient funds"
          )
      )
    )
  )

;Verifying new version
(display "New make-withdraw")
(newline)
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
(W2 40)
(W1 0)




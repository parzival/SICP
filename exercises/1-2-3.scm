; Section 1.2.3

; Setup functions
(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1)
                     )
                 (cc (- amount (first-denomination kinds-of-coins)) 
                     kinds-of-coins
                     )
                 )
              )
        )
  )

(define (first-denomination kinds-of-coins) 
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10) 
        ((= kinds-of-coins 4) 25) 
        ((= kinds-of-coins 5) 50)
        )
  )

; Demonstrating count-change
(count-change 100)

; Ex 1.14 
; Showing the tree for a procedure

; Show the tree that results from calling 
(count-change 11)

; State the orders of growth for this procedure in space and time.

; Ex 1.15
; Characterizing order of procedure

(define (cube x)
  (* x x x)
  )

(define (p x)
  (- (* 3 x) (* 4 (cube x)))
  )

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))
      )
  )

; a. Find the number of times p is called when calculating (sine 12.15).

(sine 12.15) 

; b. State the order of growth in space and time used to calculate (sine a).

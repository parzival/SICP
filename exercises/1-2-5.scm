; SECTION 1.2.5

; Ex 1.20.
(define (gcd a b) 
  (if (= b 0)
      a
      (gcd b (remainder a b))
      )
  )
; 
; If this function is interpreted using normal order, display how the evaluation proceeds and determine how many calls to (remainder) will occur in (gcd 206 40). Use the substitution model.  Also determine how many calls to remainder would be made using applicative order. 'if' is treated as per Ex 1.5.


; For normal order:
; (gcd 206 40)
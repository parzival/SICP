; SECTION 1.2.5

; Ex 1.20.
;(define (gcd a b) 
;  (if (= b 0)
;      a
;      (gcd b (remainder a b))
;      )
;  )
; 
; If this function is interpreted using normal order, display how the evaluation proceeds and determine how many calls to (remainder) will occur in (gcd 206 40). Use the substitution model.  Also determine how many calls to remainder would be made using applicative order. 'if' is treated as per Ex 1.5.


; For normal order:

; Arguments will be shown on separate lines, as they will quickly grow.
 (gcd 206 
      40
      )      
; (if (= b 0) a (gcd b (remainder a b)))
 (if (= 40 0) 206 (gcd 40 (remainder 206 40))) ;substitute body
 (gcd 40 
      (remainder 206 40)
      )
 (if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
 ; (if (= 6 0) ...  ; rem cc: 1
 (gcd (remainder 206 40) 
      (remainder 40 (remainder 206 40))
      )
 (if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
 ; (if (= (remainder 40 6) 0) ... ; rem cc: 2
 ; (if (= 4 0) ...  ; rem cc: 3
 (gcd (remainder 40 (remainder 206 40))
      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
      )
 (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
 ; (if (= (remainder (remainder 206 40) (remainder 40 6)) 0) ... ; rem cc: 4
 ; (if (= (remainder 6 4) 0) ... ; rem cc: 6
 ; (if (= 2 0) ...  ; rem cc: 7
 (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
      )
 (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
 ; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ...
 ; (if (= (remainder (remainder 40 6) (remainder 6 (remainder 40 6))) 0) ...; rem cc: 10
 ; (if (= (remainder 4 (remainder 6 4)) 0) ... ; rem cc: 12
 ; (if (= (remainder 4 2) 0) ... ; rem cc: 13
 ; (if (= 0 0) ...  ; rem cc: 14
 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))  ; argument a from the last call to gcd
 (remainder 6 (remainder 40 6)) ; rem cc: 16
 (remainder 6 4) ; rem cc: 17
 2 ; rem cc: 18
 
; This gives the result for applicative order, by using the Scheme interpreter :
 
; It's okay to delete/ignore this section if (trace) is not supported.
(require trace)
 
(define (gcd a b) 
  (if (= b 0)
      a
      (gcd b (rem-traced a b))
      )
  )

; Defining this function in this file ensures it will be traced.
(define (rem-traced a b)
  (remainder a b)
  )

(gcd 206 40) 

; 4 calls to remainder 
; Tests for generic arithmetic packages
; These require equ?, =zero? to be defined

(load-from-lib "test-functions.scm")

(define (logical-operator-tests)
  
  (define (logic-tests)
    ; These are intentionally written in a varying style
    (displayln "Performing logic tests")
    
    (define rat_2-3 (make-rational 2 3))
    (define com_3-4 (make-complex-from-real-imag 3 4))
    (define com_m-a (make-complex-from-mag-ang 5 (atan 4 3)))
    ; Testing self-equality
    (define seq_list (list (equ? 2 2)
                           (equ? rat_2-3 rat_2-3)
                           (equ? com_3-4 com_3-4)
                           (equ? com_m-a com_m-a)
                           )
      )
    
    (test-true (lambda () (check-all seq_list true?)) "Self-equality")
    
    (test-true (lambda () (check-all (list (equ? 1 0)
                    (equ? rat_2-3 (make-rational 1 12))
                    (equ? com_3-4 (make-complex-from-real-imag -1 5))
                    (equ? com_m-a (make-complex-from-mag-ang 2 3))
                    )
              false?))
               "Non-equal values"
              )
    
    ; Testing equality with equal in value but non-identical objects
    (let ((test_list  (list (list rat_2-3 (make-rational 2 3))
                            (list com_3-4 (make-complex-from-real-imag 3 4))
                            (list com_m-a (make-complex-from-mag-ang 5 (atan 4 3)))
                            (list com_3-4 com_m-a)
                            )
                      )
          (test_function (lambda(value-list) (equ? (car value-list) (cadr value-list)))
                         )
          )
      (test-true (lambda () (check-all test_list test_function)) "Equal-valued non-identical")
      )
    
    ; Testing =zero? function
    (define zero_list (list  0
                             (make-rational 0 1)
                             (make-complex-from-real-imag 0 0)
                             (make-complex-from-mag-ang 0 0) 
                             )
      )
    
    (test-true (lambda () (check-all zero_list =zero?)) "Zero")
    
    (let ((non-zero_list (list  1
                                rat_2-3
                                com_3-4
                                )
                         )
          )
      (test-true (lambda () (check-all non-zero_list (lambda(v) (not (=zero? v))))) "Non-zero values")
      )
    )
  
  (logic-tests) ; equ?, =zero? 
  'finished
  )


; Tests for basic properties of arithmetic.
; Test values marked with a * indicate those in which the 'expected' value is computed, and
; could result in uncaught errors.

(define (arith-property-tests zero one n1 n1-ai n1-mi n2)
  ; Basic addition/subtraction properties 
  ; zero: the additive identity
  ; one: the multiplicative identity
  ; n1: any given number (preferably not zero or one)
  ; n1-ai: the additive inverse of n1
  ; n1-mi: the multiplicative inverse of n1
  ; n2: any given number (preferably distinct from n1)
  (displayln "Basic property tests:")
  (test-true (lambda () (=zero? zero)) 
   "zero is =zero?"
   )                                   
  (test-false (lambda () (=zero? one)) 
   "zero is not one"
   )                                  
  (test-equ (lambda () n1) n1 
    "a value equals itself"
   )                                           
  (test-false (lambda () (equ? zero one))
    "non-equal values are not equal"
    )                                
  (displayln "Addition & Subtraction")
  (test-equ (lambda () (add zero n1)) n1
    "additive identity works"
    )                               
  (test-equ (lambda ()(add n1 n2)) (add n2 n1) ; *
    "addition commutes"
    )                         
  (test-equ (lambda () (add (add n1 n2) one)) (add n1 (add n2 one)) ; *
    "addition is associative"
    )    
  (test-true (lambda () (=zero? (add n1 n1-ai)))
    "additive inverse works"
    )                        
  (test-equ (lambda () (add n1 n1-ai)) zero 
    "testing additive inverse using equ"
    )                           
  (test-equ (lambda () (sub n1 zero)) n1
    "subtraction satisfies additive identity"
    )                                
  (test-equ (lambda () (sub zero n1)) n1-ai
    "subtraction from zero yields inverse"
    )                             
  (displayln "Multiplication & Division")
  (test-equ (lambda () (mul n1 one)) n1
    "multiplicative identity works"
    )                                 
  (test-equ (lambda () (mul n1 n2)) (mul n2 n1)         ; *
    "multiplication commutes"
    )                         
  (test-equ (lambda () (mul (mul n1 n2) n1-ai)) (mul n1 (mul n2 n1-ai))     ; *
    "multiplicative associative property holds"
    )
  (test-equ (lambda () (mul n1 n1-mi)) one
    "multiplicative inverse works"
    )                             
  (test-true (lambda () (=zero? (mul n1 zero)))
    "multiplication by zero is zero"    
    ) ; that this should hold follows from previous properties
  (test-equ (lambda () (div n1 one)) n1
    "multiplicative identity works for division"
    )                               
  (test-true (lambda () (=zero? (div zero n1)))
    "additive identity =zero? when divided"
    )                          
  (test-equ (lambda () (div one n1)) n1-mi 
    "division can produce the multiplicative inverse"
    )
  (test-equ (lambda () (mul n1 (add n2 n1-ai))) (add (mul n1 n2) (mul n1 n1-ai))  ; *
    "distributive property holds"
    )
  )

; All values in these tests are computed 

(define (scheme-number-arith-tests)
  (displayln "Scheme Number arithmetic tests")
  (let ((zero (make-scheme-number 0))
        (one  (make-scheme-number 1))
        (s1   (make-scheme-number 4))
        (s1-ai  (make-scheme-number -4))
        (s1-mi (make-scheme-number 0.25))
        (s2   (make-scheme-number 7.47))
        (s3   (make-scheme-number 2.8103e15)) ; See how large this can be
        (s4  (make-scheme-number 12))
        )
    (arith-property-tests zero one s1 s1-ai s1-mi s2)
    ; Test for correct answers - these depend on the exact values given above
    (displayln "Tests for correct answers")
    ; Addition
    (test-equ (lambda () (add s1 one)) 5)
    (test-equ (lambda () (add s2 s1)) 11.47) 
    (test-equ (lambda () (add s2 s1)) (+ 4 7.47)) ; compare to previous test
    (test-equ (lambda () (add s3 s2)) (+ 2.8103e15 7.47))
    (test-false (lambda () (equ? (add s3 one) s3)))
    ; Subtraction
    (test-equ (lambda () (sub s1 one)) 3)
    (test-equ (lambda () (sub s1-ai s2)) -11.47)    
    (test-equ (lambda () (sub s1-ai s2)) (- -4 7.47)) ; compare to previous test
    (test-equ (lambda () (sub s3 s2)) (- 2.8103e15 7.47))
    (test-equ (lambda () (add one (sub s3 one))) s3)
    ; Multiplication
    (test-equ (lambda () (mul s1 s1-ai)) -16)
    (test-equ (lambda () (mul s2 s1))  29.88)
    ; Division
    (test-equ (lambda () (div s4 s1)) 3)
    (test-equ (lambda () (div (mul s2 s3) s3)) s2) 
    )
  )

(define (rational-arith-tests)
  (displayln "Rational number arithmetic tests")
  (let ((zero (make-rational 0 1))
        (one  (make-rational 1 1))
        (r1   (make-rational 6 17))
        (r1-ai  (make-rational -6 17))
        (r1-mi (make-rational 17 6))
        (r2   (make-rational 3 7))
        (r3   (make-rational 15 34))
        (r4   (make-rational -4 7))
        )
    (arith-property-tests zero one r1 r1-ai r1-mi r2)
    ; Test for correct answers - these depend on the exact values given above
    (displayln "Tests for correct answers")
    ; Addition
    (test-equ (lambda () (add r1 one)) (make-rational 23 17))
    (test-equ (lambda () (add r2 r1)) (make-rational 93 119)) 
    ; Subtraction
    (test-equ (lambda () (sub r1 one)) (make-rational -11 17))
    (test-equ (lambda () (sub r1-ai r2)) (make-rational 93 -119))
    ; Multiplication
    (test-equ (lambda () (mul r1 r2)) (make-rational 18 119)) 
    ; Division
    (test-equ (lambda () (div r1 r3)) (make-rational 4 5))
    (test-equ (lambda () (div r2 r4)) (make-rational -3 4))
    )
  )
 

(define (complex-arith-tests)
  (displayln "Complex number arithmetic tests")
  (let ((zero  (make-complex-from-mag-ang 0 0))
        (one   (make-complex-from-mag-ang 1 0))
        (c1    (make-complex-from-mag-ang 2 1.0))
        (c1-ai (make-complex-from-mag-ang 2 (- 1.0 pi)))
        (c1-mi (make-complex-from-mag-ang (/ 1 2) -1.0))
        (c2    (make-complex-from-mag-ang 2.5 0.71))
        )
    (displayln "Mag-angle")
    (arith-property-tests zero one c1 c1-ai c1-mi c2)
    )
  (let ((zero  (make-complex-from-real-imag 0 0))
        (one   (make-complex-from-real-imag 1 0))
        (c1    (make-complex-from-real-imag 2 3))
        (c1-ai (make-complex-from-real-imag -2 -3))
        (c1-mi (make-complex-from-real-imag (/ 2 13) (/ -3 13)))
        (c2    (make-complex-from-real-imag 5 -7))
         )
    (displayln "Real-imag")
    (arith-property-tests zero one c1 c1-ai c1-mi c2)
        )
  (let (
        (c2    (make-complex-from-real-imag  2 2))
        (c3    (make-complex-from-real-imag -3 2))
        (r1    (make-complex-from-real-imag 5 0))
        (r2    (make-complex-from-mag-ang 4 pi))
        (z1    (make-complex-from-real-imag 0 2.5))
        )
    ; Test for correct answers - these depend on the exact values given above
    ;(test-true (angle (make-complex-from-real-imag 0 0))) ; Verify that taking the angle of a (0,0) number does not yield an error
    (displayln "Tests for correct answers")
    ; Addition
    (test-equ (lambda () (add r1 r2)) (make-complex-from-real-imag 1 0))
    (test-equ (lambda () (add r1 z1)) (make-complex-from-real-imag 5 2.5))
    (test-equ (lambda () (add c2 c3)) (make-complex-from-real-imag -1 4))
    ; Subtraction
    (test-equ (lambda () (sub r1 r2)) (make-complex-from-mag-ang 9 0))
    (test-equ (lambda () (sub r1 z1)) (make-complex-from-real-imag 5 -2.5))
    (test-equ (lambda () (sub c2 c3)) r1)
    ; Multiplication
    (test-equ (lambda () (mul r1 r2)) (make-complex-from-real-imag -20 0))
    (test-equ (lambda () (mul r1 z1)) (make-complex-from-real-imag 0 12.5))
    (test-equ (lambda () (mul c2 c3)) (make-complex-from-real-imag -10 -2))
    ; Division
    (test-equ (lambda () (div r1 r2)) (make-complex-from-real-imag (/ -5 4) 0))
    (test-equ (lambda () (div r1 z1)) (make-complex-from-real-imag 0 -2))
    (test-equ (lambda () (div c2 c3)) (make-complex-from-real-imag (/ -2 13) (/ -10 13)))
    )
  )


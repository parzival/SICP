(require rackunit)
(require rackunit/text-ui)

(define complex-delta 1e-10)


(define (in-cdelta? a b)
  (<= (abs (- a b)) complex-delta)
  )

(define (complex-= a b)
  (and (in-cdelta? (real-part a) (real-part b))
       (in-cdelta? (imag-part a) (imag-part b))
       )
  )

(define-binary-check (check-complex= complex-= actual expected))

; Testing

(define cnumber-tests 
  (test-suite
   "Complex number interface"
   (let ((a  (make-from-real-imag 4 3))
         (b  (make-from-mag-ang 5 0.64350110879))
         )
     (test-case
      "Real and imaginary parts"
      (check-= (real-part a) 4 complex-delta)
      (check-= (imag-part a) 3 complex-delta)
      (check-= (real-part b) 4 complex-delta)
      (check-= (imag-part b) 3 complex-delta)
      )
     (test-case
      "Magnitude and angle"
      (check-= (magnitude a) 5 complex-delta)
      (check-= (angle a) 0.64350110879 complex-delta)
      (check-= (magnitude b) 5 complex-delta)
      (check-= (angle b) 0.64350110879 complex-delta)
      )
     )
   )
  )

; Arithmetic tests

(define math-tests 
  (test-suite
   "Complex number arithmetic tests"
   (let ((zero  (make-from-mag-ang 0 0))
         (one   (make-from-mag-ang 1 0))
         (c1    (make-from-mag-ang 2 1.0))
         (c1-ai (make-from-mag-ang 2 (+ 1.0 pi)))
         (c1-mi (make-from-mag-ang (/ 1 2) -1.0))
         (c2    (make-from-real-imag 2 2))
         (c3    (make-from-real-imag -3 2))
         (r1    (make-from-real-imag 5 0))
         (r2    (make-from-mag-ang 4 pi))
         (z1    (make-from-real-imag 0 2.5))
         )
     (test-case 
      "Addition basic properties"
      ; Basic properties
      (check-complex= (add-complex zero c1) c1)
      (check-complex= (add-complex c1 c2) (add-complex c2 c1))
      (check-complex= (add-complex c1 (add-complex c2 c3)) (add-complex (add-complex c1 c2) c3))
      (check-complex= (add-complex c1 c1-ai) zero)
      )
     (test-case 
      "Addition calculation"
      ; Check for correct answers
      (check-complex= (add-complex r1 r2) (make-from-real-imag 1 0))
      (check-complex= (add-complex r1 z1) (make-from-real-imag 5 2.5))
      (check-complex= (add-complex c2 c3) (make-from-real-imag -1 4))
      )
     (test-case
      "Subtraction properties"
      (check-complex= (sub-complex c1 zero) c1)
      (check-complex= (sub-complex zero c1) c1-ai)
      )
     (test-case
      "Subtraction calculation"
      (check-complex= (sub-complex r1 r2) (make-from-mag-ang 9 0))
      (check-complex= (sub-complex r1 z1) (make-from-real-imag 5 -2.5))
      (check-complex= (sub-complex c2 c3) r1)
      )
     (test-case
      "Multiplication properties"
      ; Basic properties
      (check-complex= (mul-complex c1 one) c1)
      (check-complex= (mul-complex c1 c2) (mul-complex c2 c1))
      (check-complex= (mul-complex c1 (mul-complex c2 c3)) (mul-complex (mul-complex c1 c2) c3))
      (check-complex= (mul-complex c1 c1-mi) one)
      (check-complex= (mul-complex c1 zero) zero) ; This can fail for some implementations
      )
     (test-case
      "Multiplication calculations"
      ; Check for correct results
      (check-complex= (mul-complex r1 r2) (make-from-real-imag -20 0))
      (check-complex= (mul-complex r1 z1) (make-from-real-imag 0 12.5))
      (check-complex= (mul-complex c2 c3) (make-from-real-imag -10 -2))
      )
     (test-case
      "Division properties"
      (check-complex= (div-complex c1 one) c1)
      (check-complex= (div-complex zero c1) zero) ; This can fail too.
      )
     (test-case
      "Division calculation"
      (check-complex= (div-complex r1 r2) (make-from-real-imag (/ -5 4) 0))
      (check-complex= (div-complex r1 z1) (make-from-real-imag 0 -2))
      (check-complex= (div-complex c2 c3) (make-from-real-imag (/ -2 13) (/ -10 13)))
      )
     )
   )
  )                     

(define (run-all-tests . verbosity)
  (let ((test-verbosity (if (null? verbosity) 'normal (car verbosity)))
        ) 
    (run-tests cnumber-tests test-verbosity)
    (run-tests math-tests test-verbosity)
    )
  )


(load "../library/complex_num_tests.rkt")

(define square sqr)

; Examples of checks
(displayln "Check examples")
(check-= 5 5 0.1) ; passes, nothing happens (no output)
(check-= 5 6 0.1) ; generates failure
(check-= 'porcupine 7 0.1) ; signals error, continues

; Complex arithmetic operations
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))
                       )
  )

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))
                       )
  )

(define (mul-complex z1 z2) 
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))
                     )
  )

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))
                     )
  )


; Ben's representation
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z))))
  )
(define (angle z)
  (atan (imag-part z) (real-part z))
  )
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a)))
  )

(displayln "Testing Ben's representation")

(run-all-tests 'verbose)


; Alyssa's representation
(define (real-part z) 
  (* (magnitude z) (cos (angle z)))
  )
(define (imag-part z) 
  (* (magnitude z) (sin (angle z)))
  )
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y))) (atan y x))
  )
(define (make-from-mag-ang r a) (cons r a))

(displayln "Testing Alyssa's representation")
(run-all-tests)



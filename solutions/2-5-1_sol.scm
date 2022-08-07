; Section 2.5.1

(define (load-from-lib file)
  (load (string-append lib-path file))
  )
(define lib-path "../library/") ; set library directory as needed

; Table operations

; Racket/PB only
(load-from-lib "racket/data-tables.rkt")
;(require trace) ; Used for 2.77 - Note: Do not load gen-arith-tests if using trace

; Other Scheme implementations
;(load-from-lib "data-tables.scm")


; NOTE: Depending on Scheme implementation, editing test-functions.scm may be required
(load-from-lib "gen-arith-tests_v1.scm")

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; Generic rules applications

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags)
           )
          )
      )
    )
  )

; Arithmetic generic functions

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; Arithmetic packages

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)
    )
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y)))
       )
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y)))
       )
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y)))
       )
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y)))
       )
  (put 'make 'scheme-number
       (lambda (x) (tag x))
       )
  'done
  )

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n)
  )

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x))
                 )
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done
  )

(define (make-rational n d)
  ((get 'make 'rational) n d)
  )

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done
  )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
  )

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a)
  )

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
        (atan (imag-part z) (real-part z))
    )
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done
  )

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done
  )

; Complex selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(displayln "Installing packages")
(display "Rectangular package ... ")
(install-rectangular-package)
(display "Polar package ... ")
(install-polar-package)
(display "Complex number package ...")
(install-complex-package)

(display "Scheme-number package ... ")
(install-scheme-number-package)
(display "Rational package ... ")
(install-rational-package)

(define (show-math ops solutions)
  (for-each (lambda(op-and-solution) 
              (display (car op-and-solution))
              (display " = ")
              (display (cdr op-and-solution))
              (newline)
              )
            (map (lambda(x y) (cons x y)) ops solutions)
            )
  )


(define (show-basic-arithmetic)
  (displayln "Demonstrating basic arithmetic operations")
  ; 'Ordinary' Scheme Numbers
  (let ((op1 "0 + 2")
        (s1 (add (make-scheme-number 0) (make-scheme-number 2))) ; 2
        (op2 "11 - 3.53")
        (s2 (sub (make-scheme-number 11) (make-scheme-number 3.53))) ; 7.47
        (op3 "5 * 13.3")
        (s3 (mul (make-scheme-number 5) (make-scheme-number 13.3))) ; 66.5 
        (op4 "-56 / 4")
        (s4 (div (make-scheme-number -56) (make-scheme-number 4))) ; -14
        )
    (displayln "Scheme numbers: ")
    (show-math (list op1 op2 op3 op4) (list s1 s2 s3 s4))
    (newline)
    )
  ; Rational numbers
  (let ((op1 "(3:5) + (4:10)")
        (s1 (add (make-rational 3 5) (make-rational 4 10)))  ; 1/1
        (op2 "(6:7) - (8:13)")
        (s2 (sub (make-rational 6 7) (make-rational 8 13)))  ; 22/91
        (op3 "(5:4) * (2:5)")
        (s3 (mul (make-rational 5 4) (make-rational 2 5)))   ; 1/2
        (op4 "(-2:3) / (9:14)")
        (s4 (div (make-rational -2 3) (make-rational 9 14))) ; -28/27
        )
    (displayln "Rational Numbers: ")
    (show-math (list op1 op2 op3 op4) (list s1 s2 s3 s4))
    (newline)
    )
  ; Complex numbers (rectangular)
  (let ((op1 "(5 + 3i) + (5 - 2i)")
        (s1 (add (make-complex-from-real-imag 5 3) (make-complex-from-real-imag 5 -2)))  ; (10,1)
        (op2 "(2 + 7i) - (0.5 + i)")
        (s2 (sub (make-complex-from-real-imag 2 7) (make-complex-from-real-imag 0.5 1))) ; (1.5,6)
        (op3 "(4 + 3i) * (7 + 8i)")
        (s3 (mul (make-complex-from-real-imag 4 3) (make-complex-from-real-imag 7 8)))   ; 53.15e^1.495i
        (op4 "(-5 - 9i) / (1 + 0i)")
        (s4 (div (make-complex-from-real-imag -5 -9) (make-complex-from-real-imag 1 0))) ; 10.29e^-2.078i
        )
    (displayln "Complex Numbers (rectangular): ")
    (show-math (list op1 op2 op3 op4) (list s1 s2 s3 s4))
    (newline)
    )
  ; Complex numbers (polar)
  (let ((op1 "6e^i + 2e^-i")
        (s1 (add (make-complex-from-mag-ang 6 1) (make-complex-from-mag-ang 2 -1)))      ; (4.322, 3.366)
        (op2 "3e^2i - e^8i")
        (s2 (sub (make-complex-from-mag-ang 3 2) (make-complex-from-mag-ang 1 8)))       ; (-1.10, 1.738)
        (op3 "e^0.25i * 5e^-4.71i")
        (s3 (mul (make-complex-from-mag-ang 1 0.25) (make-complex-from-mag-ang 5 -4.71))); 5e^-4.46i
        (op4 "9e^-3i / 3e^2i")
        (s4 (div (make-complex-from-mag-ang 9 -3) (make-complex-from-mag-ang 3 2)))      ; 3e^-5i
        )
    (displayln "Complex Numbers (polar): ")
    (show-math (list op1 op2 op3 op4) (list s1 s2 s3 s4))
    (newline)
    )
  )

(show-basic-arithmetic) ; optional, to ensure system is working before starting

; Ex 2.77.
; Operations on complex numbers

(define z (cons 'complex (cons 'rectangular (cons 3 4))))

; Comment out to avoid error
; (magnitude z)

(displayln "Installing new complex number operations ...") 
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; Describe the process involved when (magnitude z) is called, where z is a complex number object.

; Verifying
(newline)
(displayln "Verifying magnitude of z")

(magnitude z)

; Note: Trace line numbers are from original file and may not match the ones in this file, as it has been slightly reformatted.
; Results of trace:
;     Procedure                        Arguments
;(.../2-5-1.scm:208:9 magnitude>      (complex rectangular 3 . 4))
; (.../2-5-1.scm:16:9 apply-generic>  magnitude (complex rectangular 3 . 4))
; (.../2-5-1.scm:208:9 magnitude>     (rectangular 3 . 4))
; (.../2-5-1.scm:16:9 apply-generic>  magnitude (rectangular 3 . 4))
; (.../2-5-1.scm:157:11 magnitude>   (3 . 4))
;  (.../2-5-1.scm:154:11 real-part>  (3 . 4))
;  (.../2-5-1.scm:155:11 imag-part>  (3 . 4))

; Result from magnitude: 5

; Ex 2.78.
; Using Lisp built-in types

; Modify the arithmetic package to handle ordinary numbers

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)
      )
  )

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (error "Bad tagged datum -- TYPE-TAG" datum)
    )
  )

(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else (error "Bad tagged datum -- CONTENTS" datum))
    )
  )


; Testing

(newline)
(displayln "Testing internal numbers for scheme-number")
(define s_1 (make-scheme-number 1))
(define s_2 (make-scheme-number 2))
(define s_3 (make-scheme-number 3))
(define s_4 (make-scheme-number -4))

; if Scheme numbers are still represented as pairs, these will fail (with an error)
(check-= (add s_1 s_2) s_3)  
(check-= (sub 14 11.3) (- 14 11.3)) 
(check-= (mul s_3 s_4)  -12)
(check-= (div 16 s_3) (/ 16 3))
(check-= (add s_1 5) (mul s_2 s_3))

(show-basic-arithmetic)

; Ex 2.79.
; Adding an equality operation

; Add an equ? operator to the arithmetic package
; Must work for ordinary, rational, and complex numbers

(define (equ? a b) (apply-generic 'equ? a b))


(define real-epsilon 1e-10)

(define (close-enough? a b)
  (<= (abs (- a b)) real-epsilon)
  )

(displayln "Adding equ? to package")
; Ordinary/Scheme numbers
(put 'equ? '(scheme-number scheme-number)
     (lambda (a b)  (close-enough? a b))
     )

; Rationals
(define (update-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'equ? '(rational rational)
       (lambda (a b) (= (* (numer a) (denom b)) (* (numer b) (denom a))))
       )
  )

(update-rational-package)

; Complex
(put 'equ? '(complex complex)
     (lambda (a b) (and (close-enough? (real-part a) (real-part b))
                        (close-enough? (imag-part a) (imag-part b))
                        )
       )                                    
     )

; Testing

(newline)
(displayln "Testing equ? operation")

(define r2-3 (make-rational 2 3))
(define r1-12 (make-rational 1 12))
(define r2-1 (make-rational 2 1))

(define c3-4 (make-complex-from-real-imag 3 4))
(define c2rpi2 (make-complex-from-mag-ang 2 (/ pi 2)))
(define c4-4 (make-complex-from-real-imag 4 4))


; Testing equality with 'equal' values that are not equal in the system
(newline)
(displayln "Testing equ? with 'equal' values that are entered differently.")
(newline)

; Comment out to prevent error
;(displayln "Without testing:")
;(check-equ s_2 2) ; this should pass (using previous modification to the system). 
;(check-equ 2 r2-1) ; this causes an error and exits - no method for these types

; Using test functions to handle errors
(displayln "Using testing:")
(test-equ (lambda () s_2) 2 "package Scheme numbers are equal to literal Scheme numbers")
(test-equ (lambda () r2-1) 2 "package rational number is equal to literal Scheme number")  ; error, but no exit

; Results that depend on how equ? is defined :

(test-equ (lambda () r2-3) (make-rational 16 24) "Rationals with non-reduced terms are equal")
(test-equ (lambda () (make-complex-from-real-imag -3 0)) (make-complex-from-mag-ang 3 (- pi)) "Complex rectangular and polar values are equal")  ; pi is system-defined

; Testing equality after operations
(newline)
(displayln "Testing equ? after operations")
(equ? 4 (add 2 2))
(equ? (make-rational 9 12) (add r2-3 r1-12))
(equ? (make-complex-from-real-imag -1 0) (sub c3-4 c4-4))

; Some of these tests will have an error, since =zero? isn't defined yet
(logical-operator-tests)

(newline)

; Ex 2.80.
; Adding a (zero?) operation

; Add a =zero? operator to the arithmetic package
; Must work for ordinary, rational, and complex numbers

(define (=zero? x) (apply-generic '=zero? x))

(displayln "Adding =zero? to arithmetic package")

; Ordinary numbers
(put '=zero? '(scheme-number)
     (lambda (x)  (or (= x 0) (close-enough? x 0.0)))
     )

; Rationals
(define (update-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x)))
       )
  )

(update-rational-package)

; Complex
(put '=zero? '(complex)
     (lambda (x) (=zero? (magnitude x)))
     )

(displayln "Testing equ? and =zero?") 
(logical-operator-tests)

; Extra tests for arithmetic; see included file 
(newline)
(displayln "Running additional tests on arithmetic")
(scheme-number-arith-tests)
(rational-arith-tests)
(complex-arith-tests)

; Zero angle tests
(newline)
(displayln "Exploring problems with the complex number system")

(define (complex-constructor-tests)
  (displayln "Constructing zero-length complex numbers")
  (test-for-success (lambda () (make-complex-from-mag-ang 0 0)))
  (test-for-success (lambda () (make-complex-from-real-imag 0 0)))
 )

(define (zero-angle-tests)
  (let ((z-ma (make-complex-from-mag-ang 0 0))
        (z-alt (make-complex-from-mag-ang 0 2)) 
        (z-ri (make-complex-from-real-imag 0 0))
        (c1 (make-complex-from-real-imag 3 5))
        (nc1 (make-complex-from-real-imag -3 -5))
        )
    (displayln "Checking angle of zero-length complex numbers")
    (test-equ (lambda () (angle z-ma)) 0 "polar complex number with zero values has angle of 0")
    ;(test-equ (lambda () (angle z-alt)) 0 "polar complex number with non-zero angle value has angle of 0")  ; optional condition
    (test-true (lambda () (=zero? z-alt)) "polar complex number with non-zero angle is =zero?") ; optional condition
    (test-equ (lambda () (angle z-ri)) 0 "rect. complex number with zero values has angle of 0")
    (test-equ (lambda () (angle (add c1 nc1))) 0 "result of computation with rect. numbers has angle of 0")
    )
  )

(complex-constructor-tests)
(zero-angle-tests)

; Complex number fix
(newline)
(displayln "Fix for zero-length complex numbers")

(define (update-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
(define (angle z)
    (if (and (= 0 (real-part z)) (= 0 (imag-part z))) 
        0                                                   
        (atan (imag-part z) (real-part z))
        )                                                   
    )
  (put 'angle '(rectangular) angle)
  )
;
(update-rectangular-package)

(displayln "Testing with fixed complex numbers")

(complex-constructor-tests)
(zero-angle-tests)
(complex-arith-tests)
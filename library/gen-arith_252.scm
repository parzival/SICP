; Table operations

; Racket/PB
(load-relative "racket/data-tables.rkt")

; Other Scheme
;(load-from-lib lib-path "data-tables.scm"))

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

; Predicates
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? x) (apply-generic '=zero? x))


; Values used for equality approximations
(define real-epsilon 2e-12)

(define (in-delta? a b)
  (<= (abs (- a b)) real-epsilon)
  )

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
  (put 'equ? '(scheme-number scheme-number)
       (lambda (a b)  (in-delta? a b))
       )
  (put '=zero? '(scheme-number)
       (lambda (x)  (in-delta? x 0))
       )
  'done
  )

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n)
  )


; Integers
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x)
    )
  (define (make-int x)
    (tag (inexact->exact (round x)))  ; 'Exact' numbers do not use a floating-point representation, and will always work with = 
    )
  (put 'add '(integer integer)
       (lambda (x y) (make-int (+ x y)))
       )
  (put 'sub '(integer integer)
       (lambda (x y) (make-int (- x y)))
       )
  (put 'mul '(integer integer)
       (lambda (x y) (make-int (* x y)))
       )
  (put 'div '(integer integer)
       (lambda (x y) (make-int (quotient x y)))  ; ignores remainder
       )
  ; Note that integer division is not defined; all integer division is handled by coercion, and 
  ; the drop to integers will occur if possible (once this is implemented). 
  
  ;(put 'div '(integer integer)
  ;     (lambda (x y) (make-rational x y))   ; optional alternate approach
  ;     )
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y))
       )
  (put '=zero? '(integer)
       (lambda (x) (= 0 x))
       )
  (put 'make 'integer
       make-int
       )
  'done
  )

(define (make-integer x) ((get 'make 'integer) x))

; Rationals

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
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x)))
       )
  (put 'equ? '(rational rational)
       (lambda (a b) (= (* (numer a) (denom b)) (* (numer b) (denom a))))
       )
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done
  )

(define (make-rational n d)
  ((get 'make 'rational) n d)
  )

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x)
    )
  ; internal procedure
  (define maxdenom (expt 2 16))    
  ; This uses an arbitrary value for rationalization; 
  ; Reals can only be converted to rationals if their fractional expression does not 
  ; require a denominator larger than maxdenom.
  
  (define (getdenom x)
    (define (getd-iter val f prev)
      (if (> val maxdenom)
          (truncate prev)
          (let ((frac (abs (- f (truncate f))))
                )
            (if (in-delta? frac 0)
                (truncate val)
                (let ((newfrac (/ 1 frac))
                      )
                  (getd-iter (* val newfrac) newfrac val)
                  )
                )
            )
          )
      )
    (getd-iter 1 x 1)
    )
  
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y)))
       )
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y)))
       )
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y)))
       )
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y)))
       )
  (put 'sine '(real)
       (lambda(x) (tag (sin x)))
       )
  (put 'cosine '(real)
       (lambda(x) (tag (cos x)))
       )
  (put 'sqroot '(real)
       (lambda(x) (tag (sqrt x)))
       )
  (put 'square '(real)
       (lambda(x) (tag (sqr x)))
       )
  (put 'arctan '(real)
       (lambda(x) (tag (atan x)))
       )
  (put 'arctan '(real real)
       (lambda(x y) (tag (atan x y)))
       )
  (put 'power '(real real)
       (lambda(x y) (tag (expt x y)))
       )
  (put 'equ? '(real real)
       (lambda (x y) (in-delta? x y))
       )
  (put '=zero? '(real)
       (lambda (x) (in-delta? x 0))
       )
  (put 'make 'real
       ;(lambda (x) (tag (exact->inexact x)))

       ; Chicken Scheme only (due to bug with rational exact values)
       (lambda (x) (tag (if (and (rational? x) (exact? x))
                            (/ (exact->inexact (numerator x)) (denominator x))
                            (exact->inexact x)
                            )
                        )
         )
                        
       )
  'done
  )

(define (make-real x) ((get 'make 'real) x))


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
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (a b) (and (in-delta? (real-part a) (real-part b))
                          (in-delta? (imag-part a) (imag-part b))
                          )
         )                                    
       )
  (put '=zero? '(complex)
     (lambda (x) (and (=zero? (magnitude x))))
     )
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
    (if (and (= 0 (real-part z)) (= 0 (imag-part z))) 
        0                                                   
        (atan (imag-part z) (real-part z))
        )                                                   
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

; Complex selectors (these must be defined first for MIT Scheme
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (arithmetic-demo)
  (displayln "Demonstrating basic arithmetic operations")
  ; Integers
  (define i1 (add (make-integer 1) (make-integer 3))) ; 4
  (define i2 (sub (make-integer 8) (make-integer 7))) ; 1
  (define i3 (mul (make-integer -5) (make-integer 13))) ; -65
  (define i4 (div (make-integer 12) (make-integer 2))) ; 6 
  (display (list i1 i2 i3 i4))
  (newline)
  ; Rational numbers
  (define rt1 (add (make-rational 3 5) (make-rational 4 10)))  ; 1/1
  (define rt2 (sub (make-rational 6 7) (make-rational 8 13)))  ; 22/91
  (define rt3 (mul (make-rational 5 4) (make-rational 2 5)))   ; 1/2
  (define rt4 (div (make-rational -2 3) (make-rational 9 14))) ; -28/27
  (display (list rt1 rt2 rt3 rt4))
  (newline)
  ; Real numbers
  (define rl1 (add (make-real 6) (make-real 4.5))) ; 10.5
  (define rl2 (sub (make-real 5.2) (make-real 0.75))) ; 4.45
  (define rl3 (mul (make-real 1.3) (make-real -0.6))) ; -.78
  (define rl4 (div (make-real 2) (make-real 30))) ; 0.06666...
  (display (list rl1 rl2 rl3 rl4))
  (newline)
  ; Complex numbers (rectangular)
  (define cr1 (add (make-complex-from-real-imag 5 4) (make-complex-from-real-imag 5 -2)))  ; (10,2)
  (define cr2 (sub (make-complex-from-real-imag 2 7) (make-complex-from-real-imag 0.5 1))) ; (1.5,6)
  (define cr3 (mul (make-complex-from-real-imag 4 3) (make-complex-from-real-imag 7 8)))   ; 53.15e^1.495i
  (define cr4 (div (make-complex-from-real-imag -5 -9) (make-complex-from-real-imag 1 0))) ; 10.29e^-2.078i
  (display (list cr1 cr2 cr3 cr4))
  (newline)
  ; Complex numbers (polar)
  (define cp1 (add (make-complex-from-mag-ang 6 1) (make-complex-from-mag-ang 2 -1)))      ; (4.322, 3.366)
  (define cp2 (sub (make-complex-from-mag-ang 3 2) (make-complex-from-mag-ang 1 8)))       ; (-1.10, 1.738)
  (define cp3 (mul (make-complex-from-mag-ang 1 0.25) (make-complex-from-mag-ang 5 -4.71))); 5e^-4.46i
  (define cp4 (div (make-complex-from-mag-ang 9 -3) (make-complex-from-mag-ang 3 2)))      ; 3e^-5i
  (display (list cp1 cp2 cp3 cp4))
  (newline)
  )





; Note tower numbers must modify these

(define (attach-tag type-tag contents)
  (if (number? contents)  ; remove for 
      contents            ; tower numbers
      (cons type-tag contents) 
      )
  )

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    ;((number? datum) (if (integer? datum) 
    ;                     'integer 
    ;                     'real
    ;                     )
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


(displayln "Installing packages")
(display "Scheme-number package... ")
(install-scheme-number-package)
(display "Rational package... ")
(install-rational-package)
(display "Rectangular package... ")
(install-rectangular-package)
(display "Polar package... ")
(install-polar-package)
(display "Complex number package... ")
(install-complex-package)
(displayln "done.")

; (void) ; Racket/PB 
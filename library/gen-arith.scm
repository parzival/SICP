; Generic-arithmetic package

; NOTE: The functions in this file should only be used for reference, or in the event that the exercises in previous sections were not completed.  It is highly recommended that these be replaced with your own solutions from section 2.5.2.


; Table operations

; Racket/PB
(load-relative "racket/data-tables.rkt")

; Other Scheme
;(load (string-append lib-path "data-tables.scm"))
      
; Create our operations table

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; Define Generic operations

(define rationalize-reals true) 
; Causes all reals within a certain tolerance to be rationalized. If false, only integer reals will be converted to rationals. 
; (Note: processing time may be increased when this is set to true).

; Type precedence used for generic ordering. Any type without a precedence will be considered 
; to be of the highest type (unraisable or droppable).
(define (precedence type)
  (let ((prec (get 'precedence type))
        )
    (if prec
        prec
        -1              ; indicates highest precedence
        )
    )
  )

(define (set-precedence type prec)
  (put 'precedence type prec)
  )

; Type ordering (note lower value = higher precedence)
(set-precedence 'integer 4)
(set-precedence 'rational 3)
(set-precedence 'real 2)
(set-precedence 'complex 1)

; PROCEDURE: (raise-lowest)
; Returns a list of args with the lowest argument raised, or false if none can be raised
(define (raise-lowest args)
  ; PROCEDURE: (raise-last)
  ; Returns a new argument list that raises the last possible argument that is as low or lower than lowest-type.
  (define (raise-last remaining-args current-new-args current-old-args lowest-type)
    (if (null? remaining-args)
        (if (< (precedence lowest-type) 0) ; negative precedence = can't be raised
            false
            current-new-args
            )
        (let ((current-arg (car remaining-args))
              (next-remaining (cdr remaining-args))
              (next-old-args (append current-old-args (list (car remaining-args))) )
              )
          ; new args 
          (if (> (precedence (type-tag current-arg)) (precedence lowest-type))
              ; this argument may be raisable
              (let ((new-arg (raise current-arg))
                    (current-type (type-tag current-arg))
                    )
                (raise-last next-remaining 
                            (append current-old-args (list new-arg) )
                            next-old-args
                            (if (not (eq? (type-tag new-arg) current-type)) ; don't change unless type actually changed
                                current-type
                                lowest-type
                                )
                            )
                )
              ; no need to change argument, precedence is high enough
              (raise-last next-remaining
                          (append current-new-args (list current-arg))
                          next-old-args
                          lowest-type
                          )
              )
          )
        )
    )
  
  (raise-last args '() '() 'null-type) 
    )
  
(define (apply-generic op . args)
  ; repeat, attempting to raise args if possible
  (define (apply-generic-iter op args)
    (let ((type-tags (map type-tag args))
          )
      (let ((proc (get op type-tags))
            )
        (if proc 
            (drop (apply proc (map contents args))) ; only change is here
            (let ((coerced-args (raise-lowest args))
                  )
              (if coerced-args
                  (apply-generic-iter op coerced-args)
                  (error "No method for these types" (list op type-tags))
                  )
              )
            )
        )
      )
    )
  
  (apply-generic-iter op args)      
  )

(define (attach-tag type-tag contents)
  (cons type-tag contents)
  )

(define (type-tag datum) 
  (cond
    ((pair? datum) (car datum))
    ((number? datum) (if (integer? datum) 
                         'integer 
                         'real
                         )
                     )
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

(define (raise x) 
  (let ((proc (get 'raise (type-tag x)))
        )
    (if proc
        (proc (contents x))
        x
        )
    )
  )

(define (project x) 
  (if (not (pair? x))
      false
      (let ((proc (get 'project (type-tag x)))
            )
        (if proc
            (proc (contents x))
            false
            )
        )
      )
  )

(define (drop x)
  (let ((lowered-x (project x)))
    (cond
      ((not lowered-x) x)
      ((equ? x (raise lowered-x)) (drop lowered-x))
      (else x)
      )
    )
  )

; Numeric packages

; This is used for projecting in complex (forces reversion and creation as a scheme number, to use in constructors)
(define (to-scheme-number x) (apply-generic 'to-scheme-number x))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x)
    )
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y)))
       )
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y)))
       )
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y)))
       )
  (put 'div '(integer integer)
       false ; handled by raising
       )
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y))
       )
  (put '=zero? '(integer)
       (lambda (x) (= 0 x))
       )
  (put 'make 'integer
       (lambda (x) (tag (inexact->exact x)))
       )
  (put 'raise 'integer ; to rational
       (lambda (x) (make-rational x 1))
       )
  'done
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
  (put 'equ? '(rational rational)
       (lambda (a b) (= (* (numer a) (denom b)) (* (numer b) (denom a))))
       )
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x)))
       )
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'project 'rational ; to integer 
       (lambda (x) (make-integer (quotient (numer x) (denom x))))
       )
  (put 'raise 'rational ; to real
       (lambda (x) (make-real (/ (numer x) (denom x))))
       )
  'done
  )

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x)
    )
  ; internal procedure
  (define maxdenom (expt 2 16))
  
  (define (getdenom x)
    (define (getd-iter d f prev)
      (if (> d maxdenom)
          (truncate prev)
          (let ((frac (abs (- f (truncate f))))
                )
            (if (in-delta? frac 0)
                (truncate d)
                (let ((newfrac (/ 1.0 frac))
                      )
                      (getd-iter (* d newfrac) newfrac d)
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
  (if rationalize-reals
      (put 'project 'real ; to rational
           (lambda (x)
             (let ((denom (getdenom x)))
               (if (> denom maxdenom)
                   false
                   (make-rational (truncate (* x denom)) (truncate denom)) 
                   )
               )
             )
           )
      (put 'project 'real ; to integer
           (lambda (x) (make-integer (truncate x)))
           )
      )
  
  (put 'to-scheme-number '(real)
       (lambda(x) (contents x))
       )
  
  (put 'make 'real
       (lambda (x) (tag (exact->inexact x)))
       )
  (put 'raise 'real ; to complex
       (lambda (x) (make-complex-from-real-imag x 0))
       )
  'done
  )


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  
  (define (project x)
    (make-real (to-scheme-number (real-part x)))
    ) 
  
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
  (put 'project 'complex  ; to real
       project
       )
  
  ; Selectors for complex numbers
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
 
  ; New equ could lead to infinite regression in some circumstances
  (put 'equ? '(complex complex) 
       (lambda (x y) (and (equ? (real-part x) (real-part y))
                          (equ? (imag-part x) (imag-part y))
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
    (sqroot (add (square (real-part z))
                 (square (imag-part z)))))
  (define (angle z)        
    (arctan (imag-part z) (real-part z))                                         
    )
  (define (make-from-mag-ang r a) 
    (cons (mul r (cosine a)) (mul r (sine a))))
  
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
  (define (magnitude z) (car z)) ; modified to force real value
  (define (angle z) (cdr z)) 
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqroot (add (square x) (square y)))           
          (arctan (imag-part z) (real-part z))
          )
    )
  
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



; Constructors, accessors, non-math functions

(define (make-integer x) ((get 'make 'integer) x))
(define (make-real x) ((get 'make 'real) x))
(define (make-rational n d)
  ((get 'make 'rational) n d)
  )
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
  )
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a)
  )


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; Basic arithmetic functions

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; more advanced functions
(define (sqroot x) (apply-generic 'sqroot x))
(define (square x) (apply-generic 'square x))
(define (power x y) (apply-generic 'power x y))

; trig functions - special treatment of 0,0 for use with complex values 
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan y . rest) 
  (cond
    ((null? rest) (apply-generic 'arctan y)) 
    ((= (length rest) 1)
     (let ((x (car rest)))
       (if (=zero? x)
           (cond
             ((< (to-scheme-number y) 0) (make-real (/ pi -2))) 
             ((> (to-scheme-number y) 0) (make-real (/ pi 2)))  
             (else (make-real 0)) ; This should be undefined for a general arctan, although 0 works best for conversion
             )
           (apply-generic 'arctan y x))
       )
     )
    (else (error "procedure arctan : expects 1 or 2 arguments, given : " a))
    )
  )

; Logical operators
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? x) (apply-generic '=zero? x))

; Used for equ
(define real-epsilon 2e-12)

(define (in-delta? a b)
  (<= (abs (- a b)) real-epsilon)
  )


; install packages

(display "Installing Integer Package ...")
(install-integer-package)
(display "Installing Rational Package ...")
(install-rational-package)
(display "Installing Real Package ...")
(install-real-package)
(display "Installing Complex Package ...")
(install-complex-package)
(display "Installing Rectangular Package ...")
(install-rectangular-package)
(display "Installing Polar Package ...")
(install-polar-package)



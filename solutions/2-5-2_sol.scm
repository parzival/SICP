; Section 2.5.2

;;;; START of Scheme-specific options
;;;; Depending on which system you're using these will need changing. 

(define lib-path "../library/") ;relative or absolute path

(define (load-from-lib file)
  (load (string-append lib-path file))
  )

; MIT Scheme ; 
;
;(load-option 'format) ; needed to format strings

; Chicken Scheme (R7RS) ; 

;(define false #f)
;(define true #t)

; Both MIT and Chicken (or any non-Racket) will need some or all of these defined:

;      (define (displayln s)
;        (display s)
;        (newline)
;        )

;      (define (andmap pred l)
;        (cond ((null? l) true)
;          ((pred (car l)) (andmap pred (cdr l)))
;          (else false)
;          )
;        )

;      (define (sqr x) (expt x 2))

;      (define pi 3.141592653589793)

;      (define (remove* rlist l)
;        (if (null? rlist)
;            l
;            (remove* (cdr rlist) (delete (car rlist) l))
;            )
;        )

; Load library files; different Scheme implementations may need to change files loaded by
; these (data-tables and test-functions).

(load-from-lib "gen-arith_252.scm")
(load-from-lib "gen-arith-tests_v2.scm")

;;;; END of Scheme-specific options


; Pre-test to ensure old system is loaded and working (optional)
;(run-snumber-arith-tests)

; Defining these allows the same test to be used with the later tower types.
(define (make-integer x)
  (make-scheme-number x)
  )

(define (make-real x)
  (make-scheme-number x)
  )

; Generic rules applications, using new rules

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args))
                    )
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1))
                      )
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2)
                         )
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2))
                         )
                        (else
                         (error "No method for these types"
                                (list op type-tags)
                                )
                         )
                        )
                  )
                )
              (error "No method for these types"
                     (list op type-tags)
                     )
              )
          )
      )
    )
  )

; No changes to tag & type functions

; Coercion routines
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))


(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (rational->complex r)
  ; numer/denom from rational package
  (define (numer x) (car x))
  (define (denom x) (cdr x)) 
  
  (let ((val (contents r))
        )
    (make-complex-from-real-imag (/ (numer val) (denom val)) 0)
    )
  )

(put-coercion 'rational 'complex rational->complex)
(put-coercion 'scheme-number 'complex scheme-number->complex)

; Run tests
(run-snumber-arith-tests)

(displayln "Pre-tests complete")
(newline)

; Ex 2.81.
; Coercion for same typed arguments

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z) 
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number
              ) 
(put-coercion 'complex 'complex complex->complex)

; Verifying with a new operation

(define (exp x y) (apply-generic 'exp x y))

;; following added to Scheme-number package
(define (update-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)
    )
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y)))) ; using primitive expt
  )

(update-scheme-number-package)

; A few examples to demonstrate that coercion is working
(newline)
(displayln "Verifying coercion")

; Values used in tests
(define s3 (make-scheme-number 3))
(define s2 (make-scheme-number 2))
(define c3-4 (make-complex-from-real-imag 3 4))
(define cn3-4 (make-complex-from-real-imag -3 4))
(define r2-3 (make-rational 2 3))


(mul c3-4 s2)  ; a complex polar number (10^0.9273i)
(add s3 cn3-4) ; complex rect number (0 + 4i)
(equ? s3 c3-4) ; false, should be no error
(equ? (sub c3-4 cn3-4) (make-scheme-number 6)) ; expect true
(exp s3 s2) ; 9 - still a scheme number

; What happens in these cases?
;(exp c3-4 s3)
;(exp c3-4 cn3-4)

(define (run-coercion-tests)
  (displayln "Testing coercion")
  ; Coercion works normally:
  (test-false (lambda () (equ? s3 c3-4))) ; should be no error
  (test-equ (lambda () (mul c3-4 s2) ) (make-complex-from-mag-ang 10 0.9272952180016)) ; 10e^0.93i
  (test-equ (lambda () (exp s3 s2) ) (make-scheme-number 9))
  (test-equ (lambda () (add r2-3 c3-4) ) (make-complex-from-real-imag 3.666666666666667 4)) 
  (test-equ (lambda () (add s3 cn3-4)) (make-complex-from-real-imag 0 4)); complex rect number (0 + 4i)
  
  ; Coercion causes error if types do not fit
  (test-for-failure (lambda ()(exp c3-4 cn3-4)) )
  (test-for-failure (lambda () (exp c3-4 s3)) )
  )

;(run-coercion-tests) 

; a. With these routines, what happens when a call is made on 
; two numbers that do not have a procedure defined for them?

; The coercion test is performed only when the procedure is not found. A coercion
; is performed, if possible, to make them both the same type, and the table is searched
; again.  Once they are both the same type, if these routines are installed,
; the search process will never terminate, since after being coerced to the same type
; the next apply-generic will again fail to find the procedure, and again coerce to
; the same type and search, without terminating.

;b. Must something be done with arguments of the same type in apply-generic?

; It is not necessary, as the procedures as written are all expected to work
; on arguments of the same type.  If such a procedure exists, it will be found
; and executed without coercion needed.

;c. Modify apply-generic to avoid coercion to the same type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args))
                    )
                (if (equal? type1 type2) 
                    (error "No method for this type" type1) ; avoid coercion to same type
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1))
                          )
                      (cond 
                        (t1->t2
                         (apply-generic op (t1->t2 a1) a2)
                         )
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2))
                         )
                        (else
                         (error "No method for these types"
                                (list op type-tags)
                                )
                         )
                        )
                      )
                    )
                )
              (error "No method for these types"
                     (list op type-tags)
                     )
              )
          )
      )
    )
  )

; Testing
(displayln "Testing modified apply-generic for same-type coercion")

(run-coercion-tests)


; Ex 2.82.
; Generalized number of arguments

; Modify (apply-generic) to handle multiple arguments with coercion.
; One strategy: attempt to coerce all arguments to type of first argument,
; second argument, etc.

(define (apply-generic op . args)
  (define (uncoerced t) t)
  
  ; Generates a sequence of possible coercions for the argument list.  Generates a list of lists
  (define (create-coercion-list typelist)
    (let ((coerce-map        
           (map (lambda(argstype)
                  (let ((coerce-tries (map (lambda(ctype)
                                             (if (equal? ctype argstype)
                                                 uncoerced ; proc that won't coerce
                                                 (get-coercion ctype argstype)
                                                 )
                                             )
                                           typelist
                                           )
                                      )
                        )
                    ; Test if there's an entry for each (relies on table returning false if not found)
                    (if  (andmap (lambda(x) x) coerce-tries) 
                         coerce-tries
                         'failed
                         )
                    )
                  )
                typelist
                )
           )
          )
      (remove* (list 'failed)  coerce-map) ; remove* removes all instances 
      )
    )
    
  (define (apply-generic-iter op args coercion-list)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (null? coercion-list)
                (error "No method for these types" (list op type-tags))
                (apply-generic-iter op 
                                    (map (lambda(coercion arg) (coercion arg)) ; applies coercion procedure 
                                         (car coercion-list) args
                                         )
                                    (cdr coercion-list)
                                    )
                )
            )
        )
      )
    )
  
  (apply-generic-iter op args  (create-coercion-list (map type-tag args)))     
  )

; Testing multiple-argument coercions
(newline)
(displayln "Testing coercion for arbitrary number of arguments")
(run-coercion-tests) ; no multiple-argument tests, but it's good to ensure that the old tests continue to work properly

; What situations might this approach not work for?  

; This attempts to coerce the arguments to only one type at a time, and only uses the types of the arguments.  It is possible that a mixed procedure may work when procedures using the same type do not (for example, a complex number raised to an integer power could be defined when complex raised to complex is not).  It may also be possible that the type necessary to coerce to is not one of the given arguments.

; Examples
;(angle s2) 
;(mul s3 r2-3)  

; Ex 2.83.
; Type hierarchy and (raise)

; Set up the tower type system
; Integers and reals are defined in the external arithmetic file, but not yet installed

; New versions for use with tower types
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

(define (update-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'raise '(rational) ; to real
       (lambda (x) (make-real (/ (numer x) (denom x))))
       )
  )

(newline)
(displayln "Updating/installing packages for tower types (use of raise)")
(display "Integer package ... ")
(install-integer-package)
(display "Updating rational package ... ")
(update-rational-package)
(display "Real package ...")
(install-real-package)

; Now we can use these types properly
(define (make-integer x) ((get 'make 'integer) x))
(define (make-real x) ((get 'make 'real) x))

; Verify working tower arithmetic
; (arithmetic-demo)
(run-tower-arith-tests)  ; Note Integers fail property tests 

; Define generic raise.  

(define (raise x)
  (let ((proc (get 'raise (type-tag x)))
        )
    (if proc
        (proc (contents x))
        x
        )
    )
  )

; Install for the included types (Integer, Rational, Real, Complex)

(display "Installing Raise for each type")

(put 'raise 'integer ; to rational
     (lambda (x) (make-rational x 1))
     )

(put 'raise 'rational ; to real
     (lambda (x)
       (define (num x) (car x))
       (define (den x) (cdr x))
       (make-real (/ (num x) (den x)))
       )
     )

(put 'raise 'real ; to complex
     (lambda (x) (make-complex-from-real-imag x 0))
     )

; Testing/Verification
(displayln "Demonstrating (raise) operation")
(raise (make-integer 3))
(raise (make-rational 2 3))
(raise (make-real 3))
(raise (make-complex-from-real-imag 4 5)) ; no defined result

(raise (raise (raise (make-integer 1))))

; Ex 2.84.
; Using (raise) for coercion

; Modify (apply-generic) to use (raise) to coerce.

(define (unraisable? x)
  (equal? (type-tag (raise x)) (type-tag x))
  )

(define (can-raise-to? x type)  ; checks if x can be raised to the type - returns raised x if so, else false
  (cond ((equal? (type-tag x) type) x)
        ((unraisable? x) false)
        (else
         (can-raise-to? (raise x) type)
         )
        )
  )

(define (precedence type)
  (let ((prec (get 'precedence type))
        )
    (if prec
        prec
        -1              ; indicates highest precedence
        )
    )
  )

; Type ordering (note lower value = higher type)
(put 'precedence 'integer 4)
(put 'precedence 'rational 3)
(put 'precedence 'real 2)
(put 'precedence 'complex 1)

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
            (apply proc (map contents args))
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

; Testing 
(newline)
(displayln "Testing apply-generic with raising")

(define int_2 (make-integer 2))
(define int_3 (make-integer 3))
(define rat_2-3 (make-rational 2 3))
(define rl_a (make-real 2.666666666667))
(define rl_b (make-real 3.2309687657353861))
(define com_3-4 (make-complex-from-real-imag 3 4))
(define com_12e2 (make-complex-from-mag-ang 12 2))


(test-equ (lambda () (add int_2 rl_a)) (make-real 4.666666666667)) 
(test-equ (lambda () (sub com_3-4 int_3)) (make-complex-from-real-imag 0 4))
(test-equ (lambda () (div rat_2-3 rl_a)) (make-real 0.25)) 


; Optional checks using 'ordinary' Scheme values
(add int_2 4)
(sub 17.3 int_2) 
(add 2 2)
(sub 3 (/ 4 5))

; Test basic operations still work
(run-tower-arith-tests) 

; Ex 2.85 
; Simplifying by dropping type

; Design a way to simplify numbers in the hierarchy by lowering their type

; Step 1: Create a generic 'project' operation and ensure 'equ?' is defined
; for all types. 

(define (update-integer-package)
  (put 'div '(integer integer)
       false   ; delete the integer op, allow raising to handle all division
       )
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y))
       )
  (put '=zero? '(integer)
       (lambda (x) (= 0 x))
       )
  )

(define (update-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'project 'rational ; to integer 
       (lambda (x) (make-integer (quotient (numer x) (denom x))))
       )
  )

; One difficulty: -all real values in the computer might be considered rational, but 
; converting them could require knowledge of the internal representation.

; This will take the approach that values expressible as a fraction with a large enough denominator (> 2^-16)
; can be reduced.  However, this can add a significant amount of time to calculations involving reals. 

(define rationalize-reals true)
; The alternative (apparently suggested by the text?) is that nothing with a decimal fraction can be
; treated as rational and only integers can be dropped. 

(define (update-real-package)
  (put 'equ? '(real real)
       (lambda (x y) (in-delta? x y))
       )
  (put '=zero? '(real)
       (lambda (x) (in-delta? x 0))
       )
  ; internal procedure
  (define maxdenom (expt 2 16)) 
 
 ; This will return a potential denominator for a real value, within the limit of maxdenom.  If the value can be expressible using a smaller denominator (within the delta value indicating equality of two reals), that will be returned.
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

  
  
  (if rationalize-reals
      (put 'project 'real ; to rational
           (lambda (x)
             (let ((denom (getdenom x)))
               (make-rational (truncate (* x denom)) denom) 
               )
             )
           )
      (put 'project 'real ; to integer
           (lambda (x) (make-integer (truncate x)))
           )
      )
  )


(define (update-complex-package)
  (put 'project 'complex  ; to real
       (lambda(x) (make-real (real-part x)))
       )
  )

; Project excludes bare numbers from being projected
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

(newline)
(displayln "Updating number packages for (project)")
(display "Updating integer package ...")
(update-integer-package)
(display "Updating rational package ... ")
(update-rational-package)
(display "Updating real package ... ")
(update-real-package)
(display "Updating complex package ... ")
(update-complex-package)

(define (drop x)
  (let ((lowered-x (project x)))
    (cond
      ((not lowered-x) x)
      ((equ? x (raise lowered-x)) (drop lowered-x))
      (else x)
      )
    )
  )


; Testing
(newline)
(displayln "Testing drop to lower type")

; Values that can be dropped
(test-equal (lambda () (type-tag (drop (make-rational 3 1)))) 'integer)
(test-equal (lambda () (type-tag (drop (make-real 2.0)))) 'integer)
; reals can be treated in two ways
(test-equal (lambda () (type-tag (drop (make-complex-from-real-imag 5.55 0))))
            (if rationalize-reals
                'rational
                'real
                )
            )


(newline)
(displayln "Testing numbers that can't be dropped.")
(test-equal (lambda () (type-tag (drop rat_2-3))) 'rational)
(test-equal (lambda () (type-tag (drop (make-real 0.00194549560546875)))) 'real) ; presumably not dropped
(test-equal (lambda () (type-tag (drop com_12e2))) 'complex)

(displayln "Testing numbers that might be dropped.")
; Possibly dropped, depending on how reals are rationalized

; These values may help explain how the rationalization works.
(if rationalize-reals 
    (begin
      (test-equal (lambda () (type-tag (drop (make-real 1.5)))) 'rational)          ; 3/2
      (test-equal (lambda () (type-tag (drop (make-real (/ 9.0 65536))))) 'rational)  ; 1/2^16 + 1/2^13 (larger than the next two)
      (test-equal (lambda () (type-tag (drop (make-real (/ 3.0 32768))))) 'rational)  ; 1/2^15 + 1/2^14 
      (test-equal (lambda () (type-tag (drop (make-real (/ 4.0 131072))))) 'rational) ; 2^2/2^17 = 1/2^15
      (test-equal (lambda () (type-tag (drop (make-real (/ 3.0 262144))))) 'rational) ; 1/2^18 + 1/2^16
      )
    )
(test-equal (lambda () (type-tag (drop 5.0))) 'real)                      ; Kept as is, or dropped

; No dropping in the system yet

; Create a function to check correct results and type
(define (check-answer-and-type arith-op arg1 arg2 expected-result expected-type label)
  (test-equal (lambda ()
                 (type-tag (arith-op arg1 arg2))
                 )
               expected-type
               label
               )
  )

(define (test-caat-against-list args)
  (apply check-answer-and-type args)
  )

(define computed-non-drops (list (list mul int_2 int_3 (make-integer 6) 'integer "Int x Int")  
                                 (list add int_2 rl_b  (make-real 5.2309687657353861) 'real "Int + Real")
                                 (list sub com_3-4 int_3 (make-complex-from-real-imag 0 4) 'complex "Com - Int")
                                 (list div rat_2-3 (make-rational 3 4) (make-rational 8 9) 'rational "Rat / Rat")
                                 (list div int_2 int_3 (make-rational 2 3) 'rational "Int / Int")
                                 )
  )

(define computed-drops (list (list mul rat_2-3 int_3 (make-integer 2) 'integer  "Rat x Int => Int")  
                             (list sub com_3-4 (make-complex-from-real-imag 0.12345689 4) (make-real 2.87654311) 'real "Com - Com => Real")
                             (list add com_3-4 (make-complex-from-real-imag 2 -4) (make-real 5.0) 'integer "Com + Com => Int")
                             ; Works only when using a projection to rationals
                             (list div (make-real 0.75) rat_2-3 (make-rational 9 8) 'rational "Rat / Real => Rat")
                             )
  )


(newline)
(displayln "Testing Arithmetic Results (without dropping in place)")
(displayln "Non-drops")
(for-each test-caat-against-list
          computed-non-drops
         )

(displayln "Drops")
(for-each test-caat-against-list
          computed-drops
          )

; Step 2 : modify apply-generic to simplify results

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

(newline)


(displayln "Testing Arithmetic Results (with dropping in place)")
(displayln "Non-drops")
(for-each test-caat-against-list
          computed-non-drops
         )

(displayln "Drops")
(for-each test-caat-against-list
          computed-drops
          )


; Test basic operations still work
(newline)
(displayln "Testing basic operation")
(run-tower-arith-tests)

; Ex 2.86.
; Handling complex numbers built from other numbers

; What changes are necessary?

; The complex numbers can be defined in terms of other types as long as the operations involving their parts all use generic operators.  This requires replacing (+) operations with (add), etc.  This is necessary down to the level of the polar/rectangular representations that constitute complex numbers. 

; Implement such a system.

; Define some new generic operators

; This is used for projecting in complex (forces reversion and creation as a scheme number, to use in constructors)
(define (to-scheme-number x) (apply-generic 'to-scheme-number x))

(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (sqroot x) (apply-generic 'sqroot x))
(define (square x) (apply-generic 'square x))
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
    (else (error "procedure arctan : expects 1 or 2 arguments, given : " (cons y rest)))
    )
  )

; Thanks to automatic type promotion, we only need to define these operators for reals.
(define (update-real-package)
  (put 'sine '(real)
       (lambda(x) (make-real (sin x)))
       )
  (put 'cosine '(real)
       (lambda(x) (make-real (cos x)))
       )
  (put 'sqroot '(real)
       (lambda(x) (make-real (sqrt x)))
       )
  (put 'square '(real)
       (lambda(x) (make-real (sqr x)))
       )
  (put 'arctan '(real)
       (lambda(x) (make-real (atan x)))
       )
  (put 'arctan '(real real)
       (lambda(x y) (make-real (atan x y)))
       )
  (put 'to-scheme-number '(real)
       (lambda(x) (contents x))
       )
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
    (arctan (imag-part z) (real-part z)))
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
          (arctan y x)))
  
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

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(displayln "Installing modified complex number packages")
(display "Rectangular package ... ")
(install-rectangular-package)
(display "Polar package ... ")
(install-polar-package)
(display "Complex number package ...")
(install-complex-package)
(display "Updating real package ...")
(update-real-package)

(displayln "Testing modified number package")

; Check basic operation
(complex-arith-tests)

(newline)

(displayln "Additional testing with new Complex system")

; Test equality with non-complex values
(displayln "Testing complex values mixed with non-complex values") 
(define c_r5.3 (make-complex-from-mag-ang 5.3 0))
(define c_i2 (make-complex-from-mag-ang 2 0))

(test-equ (lambda () c_r5.3) (make-real 5.3))
(test-false (lambda () (equ? c_r5.3 (make-real 4.2))))
(test-equ (lambda () c_i2) (make-integer 2))

; Test arithmetic
(displayln "Additional arithmetic testing (mixed values)")
(define com_4-4 (make-complex-from-real-imag (make-integer 4) (make-real 4)))
; other values keep previous definitions
; Check arithmetic that mixes non-complex values
(test-equ (lambda () (mul int_3 rl_a)) (make-real 8) "Real results from integer * real" ) ; 3 * 2 2/3 
(test-equ (lambda () (div rat_2-3 (make-rational 3 4))) (make-rational 8 9) "Rational results when dividing rationals") 

; complex values
(test-true (lambda () (=zero? (make-complex-from-real-imag (make-integer 0) (make-rational 0 1) ))) "Zero-valued complex (real/rational)" )
(test-equ (lambda () (make-complex-from-real-imag (make-real -1.993762038566) (make-real 14.911569121908))) (add com_3-4 com_12e2) "complex using reals")
(test-equ (lambda () (make-complex-from-real-imag (make-rational -1 1) (make-rational 0 1))) (sub com_3-4 com_4-4)  "complex using rationals and complex values")

; Note the irrationality of the 'rational' value - this works in Racket due to its implementation of gcd, but not in all varieties of Scheme
(test-equ (lambda () (mul com_4-4 (make-complex-from-mag-ang (make-rational 3 (sqrt 2)) 0))) (make-complex-from-mag-ang 12 0.7853981633974) "complex using irrational 'rational' fraction")
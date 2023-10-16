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

(run-coercion-tests) 

; a. With these routines, what happens when a call is made on 
; two numbers that do not have a procedure defined for them?

;b. Must something be done with arguments of the same type in apply-generic?

;c. Modify apply-generic to avoid coercion to the same type.


; Testing
(displayln "Testing modified apply-generic for same-type coercion")

(run-coercion-tests)


; Ex 2.82.
; Generalized number of arguments

; Modify (apply-generic) to handle multiple arguments with coercion.
; One strategy: attempt to coerce all arguments to type of first argument,
; second argument, etc.


; Testing multiple-argument coercions
(newline)
(displayln "Testing coercion for arbitrary number of arguments")
(run-coercion-tests) ; no multiple-argument tests, but it's good to ensure that the old tests continue to work properly

; What situations might this approach not work for?  

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

; Testing/Verification
(displayln "Demonstrating (raise) operation")
(raise (make-integer 3))
(raise (make-rational 2 3))
(raise (make-real 3))
;(raise (make-complex-from-real-imag 4 5)) ; no defined result

(raise (raise (raise (make-integer 1))))

; Ex 2.84.
; Using (raise) for coercion

; Modify (apply-generic) to use (raise) to coerce.

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
;(add int_2 4)
;(sub 17.3 int_2) 
;(add 2 2)
;(sub 3 (/ 4 5))

; Test basic operations still work
(run-tower-arith-tests) 

; Ex 2.85 
; Simplifying by dropping type

; Design a way to simplify numbers in the hierarchy by lowering their type

; Step 1: Create a generic 'project' operation and ensure 'equ?' is defined
; for all types. 


; One difficulty: -all real values in the computer might be considered rational, but 
; converting them could require knowledge of the internal representation.

; This will take the approach that values expressible as a fraction with a large enough denominator (2^16)
; can be reduced.  However, this can add a significant amount of time to calculations involving reals. 

(define rationalize-reals false) ; Set this true to use this; projection & dropping need to be defined as well.

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
	
	; < Insert additional changes for real >
	(if rationalize-reals
		; < Project to rational values >
		; < Only project to integer values >
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


; Implement such a system.




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
(test-true (lambda () (=zero? (make-complex-from-real-imag (make-integer 0) (make-rational 0 1) ))) )
(test-equ (lambda () (make-complex-from-real-imag (make-real -1.993762038566) (make-real 14.911569121908))) (add com_3-4 com_12e2) "complex using reals")
(test-equ (lambda () (make-complex-from-real-imag (make-rational -1 1) (make-rational 0 1))) (sub com_3-4 com_4-4)  "complex using rationals and complex values")

; Note the irrationality of the 'rational' value - this works in Racket due to its implementation of gcd, but not in all varieties of Scheme
(test-equ (lambda () (mul com_4-4 (make-complex-from-mag-ang (make-rational 3 (sqrt 2)) 0))) (make-complex-from-mag-ang 12 0.7853981633974) "complex using irrational 'rational' fraction")
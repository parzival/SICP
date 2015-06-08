; Section 2.4.3

(load "data_tables.rkt")

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(load "../library/complex-num-tests.rkt")

; Arithmetic operations
(define square sqr)

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
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z)) 
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) 
             (square (imag-part z))
             )
          )
    )
  (define (angle z) 
    (atan (imag-part z) (real-part z))
    )
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a)))
    )
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part) 
  (put 'magnitude '(rectangular) magnitude) 
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y)))
       )
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a)))
       )
  'done
  )

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z)) 
  (define (make-from-mag-ang r a) (cons r a)) 
  (define (real-part z)
    (* (magnitude z) (cos (angle z)))
    )
  (define (imag-part z)
    (* (magnitude z) (sin (angle z)))
    )
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) 
          (atan y x)
          )
    )
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part) 
  (put 'imag-part '(polar) imag-part) 
  (put 'magnitude '(polar) magnitude) 
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar 
       (lambda (x y) (tag (make-from-real-imag x y)))
       )
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a)))
       )
  'done
  )

; Generic operations
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

; Selectors for complex numbers
(define (real-part z)
  (apply-generic 'real-part z)
  ) 
(define (imag-part z) 
  (apply-generic 'imag-part z)
  ) 
(define (magnitude z) 
  (apply-generic 'magnitude z)
  ) 
(define (angle z) 
  (apply-generic 'angle z)
  )

; Constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y)
  )
(define (make-from-mag-ang r a) 
  ((get 'make-from-mag-ang 'polar) r a)
  )


(displayln "Installing complex packages")
(display "Installing rectangular...")
(install-rectangular-package)
(display "Installing polar...")
(install-polar-package)

(displayln "Testing Complex Numbers")
(run-tests complex-tests)

; Ex. 2.73.
; Symbolic differentiation using data-directed dispatch

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
  )

; Note modifications to work on a list of operands
; instead of an expression

(define (augend s) (car s))

(define (addend s) (cadr s))

(define (multiplier p) (car p))

(define (multiplicand p) (cadr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))
        )
  )

(define (=number? exp num)
  (and (number? exp) (= exp num))
  )

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
        )
  )


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else (
               (get 'deriv (operator exp)) (operands exp)
                                           var
                                           )
              )
        )
  )

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; a. Explain what was done here.  Why can't number and variable be
; part of the data-directed dispatch?


; b. Write procedures for sum & product, plus the auxiliary code
; to install them into the table.


; Testing
(define (show-basic-deriv)
  (displayln (deriv '(+ x 3) 'x))  ; 1
  (displayln (deriv '(* x y) 'x))  ; y
  (displayln (deriv '(* (* x y) (+ x 3)) 'x)) ; (2xy + 3y)
  )

(show-basic-deriv)


; c. Add a new differentiation rule 


; d. If the dispatch line was rewritten to be :
; ((get (operator exp) 'deriv) (operands exp) var)
; what changes would be necessary?


; Ex. 2.74.
; Implementing a data-directed dispatch system

; a. Implement a (get-record) procedure that will
; get an employee's record for a file.




; Testing
(newline)
(displayln "Testing personnel records.")

; Example file
(define scr-info '(division-info  (division-id scranton) (division-loc ((state PA) (zip 18510)))))
(define scr-employees '((scott (2500 office-manager)) 
                        (schrute (1200 sales)) 
                        (kapoor (2000 service-rep))
                        )
  )

(define dm-file (append (list scr-info) scr-employees))

; Interface for getting a record
(define (scranton-retrieve-record employee personnel-file)
  (let ((found-record (assoc employee personnel-file)))
    (if found-record
        (cadr found-record)
        false
        )
    )
  )

; Testing get-record with one file
(display "Kapoor's record from dm-file is ")
; Get Kapoor's record

; Attempt to get a record not existing in the file
(display "Palmer's record from dm-file is ")


; b. Implement a (get-salary) procedure that can
; get the salary information from a given employee's 
; record.


;Testing
(display "Schrute's salary is ")
; Get Schrute's salary using get-salary


; c. Implement a (find-employee-record) procedure to
; locate an employee in a list of files


;Testing
(define employee-files (list dm-file))

(display "Searching for record of Scott ... ")
; Use find-employee-record to find scott in employee-files.


; Adding another file

(define slo-info '(division-info (division-loc 'SL1)))

(define slo-employee-ids '(id-nos (Brent 0) (Keenan 1) (Roper 2)))

(define slo-salaries '(salary (600 500 1800)))
(define slo-positions '(position (general-manager jobsworth personal-assistant)))

(define wh-file (list slo-info slo-employee-ids slo-salaries slo-positions))

; Defined interface for the file
(define (install-wh-package)
  ; internal procedures
  (define (get-employee-id employee personnel-file)
    (let ((id-list (assoc 'id-nos personnel-file)))
      (if id-list
          (let ((emp-id-pair (assoc employee (cdr id-list))))
            (if emp-id-pair
                (cadr emp-id-pair)
                false
                )
            )
          (error "Couldn't locate employee id in file : " personnel-file)
          )
      )
    )
  
  (define (record-retrieval employee personnel-file)
    (let ((id (get-employee-id employee personnel-file)))
      (if id
          (let ((salary (list-ref (cadr (assoc 'salary personnel-file)) id ))
                (position (list-ref (cadr (assoc 'position personnel-file)) id ))
                )
            (list (list 'salary salary) (list 'position position))
            )
          false
          )
      )
    )
  
  (define (salary-from-record record)
    (let ((salary-pair (assoc 'salary record)))
      (if salary-pair
          (cdr salary-pair)
          (error "Cannot process record for salary : " record)
          )
      )
    )
  
  ; interface
  
  (put 'retrieve-record 'slough record-retrieval)
  (put 'retrieve-salary 'slough salary-from-record)
  )



;Testing wh-package
(display "Verifying wh-package install ... ")
(install-wh-package)


(displayln "Testing wh-file")
; Use get-record for Brent in the file

; Use get-record for someone not in the file

; Do searches across all division files

; Testing with multiple files
(display "Searching for Scott ... ")   ; person is in a file
;(find-employee-record...
(display "Searching for Roper ... ")   ; person in another file
;(find-employee-record...
(display "Searching for Bratton ... ") ; person not in any file
;(find-employee-record...


; Check that get-salary works
(display "Brent's salary is :")
;(get-salary (find-employee-record 'Brent ...
; Ought to be 600

; d. What changes are required when taking over a new company?



; Ex. 2.75
; Complex numbers using message-passing 

; Example of message passing (complex numbers)
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (sqr x) (sqr y)))
           )
          ((eq? op 'angle) (atan y x))
          ((eq? op 'print) (printf "~a + ~a i " x y)) ;; added functionality
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)
           )
          )
    )
  dispatch
  )


(define (apply-generic op arg) (arg op))

; Note that the selectors do not need to change (although apply-generic has).

; Implement (make-from-mag-ang) in this style.



; Testing

(displayln "Verifying make-from-real-imag")
(define n1 (make-from-real-imag -2 7))
(real-part n1)
(magnitude n1)

(displayln "Testing make-from-mag-ang.")
(define n2 (make-from-mag-ang 3 4))
(real-part n2)
(imag-part n2)
(magnitude n2)
(angle n2)

(newline)
(displayln "Testing complex number operations using message passing.")

(run-tests numeric-tests)

(displayln "Testing arithmetic.")
(run-tests math-tests)

; Add 'print' to mag-ang for this to work
(define (show-complex z) (apply-generic 'print z)) 

(display "n1: ")
(show-complex n1)
(newline)
(display "n2: ")
(show-complex n2)
(newline)
(display "n1 + n2: ")
(show-complex (add-complex n1 n2))
(newline)
(display "n1 / n2: ")
(show-complex (div-complex n1 n2))
(newline)


; Ex. 2.76.
; Comparing dispatch types.

; What changes would be necessary to add a new type or operation?

; What style works best for adding new types?  Adding new operations?

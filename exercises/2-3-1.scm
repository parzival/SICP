; Section 2.3.1

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))
        )
  )


; Ex 2.53.
; Describe what the interpreter response is for each expression

(list 'a 'b 'c)  

(list (list 'george)) 
(cdr '((x1 x2) (y1 y2))) 

(cadr '((x1 x2) (y1 y2))) 
(pair? (car '(a short list))) 
(memq 'red '((red shoes) (blue socks))) 

(memq 'red '(red shoes blue socks))     

; Ex 2.54.
; An equality procedure for lists
;(define built-in-equal? equal?)  ; preserve built-in form if needed 

(define (equal? a b) false )   ; override any existing version

; Testing/Observation
; Some testing functions
(define (check-true x) (displayln (if x "passed" "failed")))
(define (check-false x) (displayln (if (not x) "passed" "failed")))
(define a 'sym)
(define b 'sym)
(define c 'not-same)
(if (not (eq? a b))
    (displayln "Warning: Identical symbols not eq?")
    )

(displayln "Examining list equality")
(check-true  (equal? '(this is a list) '(this is a list)))   ; Two equal lists should be equal
(check-false (equal? '(this is a list) '(this (is a) list))) ; Two non-equal lists should not be equal
(check-true  (equal? '(this is a list) (list 'this 'is 'a 'list))) ; Lists should be equal no matter how created
(check-false (equal? '(a list) '()))  ; A non-null list should not be equal to the empty list
(check-true (equal? (list a a) (list a b)))  ; Lists should be equal when content is equal
(check-false (equal? (list a a a) (list b b))) ; Lists of different length should not be equal
(newline)

(displayln "Pairs")
(check-true (equal? (cons a c) (cons a c))) ; Pairs should be equal
(check-true (equal? '(x . y) (cons 'x 'y)))     ; Pairs constructed differently should be equal 
(check-false (equal? (cons 'a 'b) '(a b)))        ; Pairs should not equal lists

(displayln "Lists")
(check-true (equal? (list '(x y) 'z 'w) (cons '(x y) '(z w)))) ; Constructed lists should be equal
(check-true (equal? (cdr '(this is a list)) (cdr '(this is a list)))) ; Equal lists as a result of operations should be equal
(check-true (equal? (cdr '(this is a list)) '(is a list))) ; Lists should equal expected results
(check-false (equal? '() (list '())))  ; A list containing an empty list is not an empty list
; These may be implementation dependent
(equal? '() '())                                        
(equal? (cdr '(single)) '())    ; Similar, but the first empty list is the result of an expression    
(equal? '() (list ))

(displayln "Numbers")  ; Equality of numbers of varying 'type' via eq? may be implementation-dependent
(equal? 1 1)
(equal? 1 2)
(equal? '1 1)
(equal? 5 5.0)
(equal? '(1 2) (list 1 (+ 1 1)))
(equal? (* 2 3) (/ 12 2))
(equal? (* 2 3.0) (/ 12 2))
(equal? 4 (/ 12 3))
(equal? (* 2 3.0) (/ 12.0 2))
(newline)

; Ex. 2.55.
; Evaluating a 'double quote'

(car ''abracadabra)

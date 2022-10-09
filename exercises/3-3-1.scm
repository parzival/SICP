; Section 3.3.1
; Does not work in base Racket (requires Scheme R5RS for set-cdr!)

(define (displayln x)
  (display x)
  (newline)
  )

(define (check-eqv? observed expected)
  (if (eqv? observed expected)
      (displayln "Passed.")
      (begin
        (display "FAILURE: ")
        (display observed)
        (display " is not equal to ")
        (display expected)
        (display ".")
        (newline)
        )
      )
  )

; Ex. 3.12 (Exercise should be done manually -- this is only for verification)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))
      )
  )

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x
  )


; Verifying append!
(displayln "Verifying lists with append!")

(define x (list 'a 'b))
(define y (list 'c 'd)) 
(define z (append x y))

(display "z: ")
z
; (a b c d)
(display "(cdr x) after (append): ")
(cdr x)
; <response>
(define w (append! x y))

(display "w: ")
w
; (a b c d)
(display "(cdr x) after (append!): ")
(cdr x)
; <response>
(newline)

; Ex. 3.13. (Another manual exercise)

(define (make-cycle x) 
  (set-cdr! (last-pair x) x)
  x
  )

(define z (make-cycle (list 'a 'b 'c)))

;(last-pair z)

;(for-each display z) ; See what this does

;(newline)

; Ex. 3.14.

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x))) 
          (set-cdr! x y)
          (loop temp x)
          )
        )
    )
  (loop x '())
  )

(displayln "Examining mystery function")

(define v (list 'a 'b 'c 'd))

; v ; Compare v before wx is defined

(define wx (mystery v))

;What is printed for each of these?
;v
;wx

(newline)


; Ex. 3.15.
; Box and pointer structure for set-to-wow!

(define ab (list 'a 'b)) 
(define z1 (cons ab ab))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x
  )

; Verifying
(displayln "Verifying set-to-wow!")

; Checking statements in text are as expected
(display "car of z1 is cdr of z1: ")
(check-eqv? (car z1) (cdr z1))
(display "car of z2 is not cdr of z2 (failure expected): ")
(check-eqv? (car z2) (cdr z2))

z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

(newline)


; Ex. 3.16.
; Faulty Pair-counting

(define (count-pairs x) 
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x)) 
         1
         )
      )
  )


; <? Define structures that will fail ?>

(displayln "Verifying faulty pair-counter.")
; Returns 3, 4, 7, never
(check-eqv? (count-pairs <? 3 ?>) 3)
(check-eqv? (count-pairs <? 4 ?>) 4)
(check-eqv? (count-pairs <? 7 ?>) 7)
(count-pairs <? never ?>) ; Comment out after verifying no return

(newline)


; Ex. 3.17.
; Correct pair counting

; <? define count-pairs ?>

; Testing
(displayln "Testing correct pair-counter.")
; Returns 3 for all - use previously defined structures
(check-eqv? (count-pairs <? 3 ?>) 3)
(check-eqv? (count-pairs <? 4 ?>) 3)
(check-eqv? (count-pairs <? 7 ?>) 3)
(check-eqv? (count-pairs <? never ?>) 3) 

(newline)


;Ex. 3.18.
; Cycle-finding

; <? define cycle-finding procedure ?>

;Testing

(displayln "Testing cycle finder")

(define cycle-test-list
  (list  
   (list '(a b c d)                #f) ; regular non-cyclic list
   (list  (make-cycle '(a b c))    #t) ; cycle
   (list  (append '(a b c) (make-cycle '(d e f)))           #t) ; cycle does not include whole list 
   (list  (list (make-cycle '(a b)) (make-cycle '(c d e)))  #f) ; member of list is a cycle (but list is not a cycle)
   (list '(a b a c d)              #f) ; repeated value in list (not a cycle)
   (list '(a a a)                  #f) ; all list members identical (non-cycle)
   (list  (make-cycle '(a))        #t) ; cycle with only one member.
   (list  (make-cycle '(a b a d))  #t) ; repeated value (in cyclic list)
   )
  )

(define cycle-test-values (map car cycle-test-list))
(define cycle-expected-results (map cadr cycle-test-list))

(define (test-cycle-finder expected test-values)
  (let ((results (map (lambda (e o) (eqv? e (<? cycle-finder with arg o ?> )) )
                      expected
                      test-values
                      )
                 )
        )
    (if (memq #f results)
        (begin
          (display "Cycle tests failed. Test results were: ")
          (displayln tests-passed)
          )
        (displayln "All cycle tests passed.")
        )
    )
  )

(test-cycle-finder cycle-expected-results cycle-test-values)


; Ex. 3.19.
; Do the cycle-finding test in constant space


; <? redefine the cycle finding function ?>

(displayln "Testing cycle finder (constant space version)")

(test-cycle-finder cycle-expected-results cycle-test-values)


; Exercise 3.20 (Intended as manual)

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

; show environment diagrams for the following

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)

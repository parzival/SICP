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
; (b)
(define w (append! x y))

(display "w: ")
w
; (a b c d)
(display "(cdr x) after (append!): ")
(cdr x)
; (b c d)

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
v
wx

; Mystery performs a 'destructive' reversal.  A new reversed list is created, and the input to it is reduced to a single element, since its cdr is modified.
; In fact, mystery repeatedly modifies the lists it creates, only keeping the final one.


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


; Define structures that will fail 

(define three-pc '(a b c))       ; 3 pairs: ('a . ( 'b . ('c . '() )))   
(define x1 (cons 'a 'b))   
(define four-pc (list x1 x1))   ; 3 pairs: 2 in ( x1 ( x1 . '()) and x1 itself [ ('a . 'b) ]
(define xc (cons x1 x1))
(define y1 (cons 'a '())) ; list
(define y2 (cons y1 y1))
(define seven-pc (cons y2 y2))   ; 3 pairs: ( y2 . y2 ), (y1 . y1 ), and y1 itself [ ('a . '() ) ]
(define i1 (cons 0 1))
(define i2 (cons i1 2))
(define i3 (cons i2 3))
(set-car! i1 i3)            ; This creates a cycle (could also use make-cycle)
(define inf-pc i1)

(displayln "Verifying faulty pair-counter.")
; Returns 3, 4, 7, never
(check-eqv? (count-pairs three-pc) 3)
(check-eqv? (count-pairs four-pc) 4)
(check-eqv? (count-pairs seven-pc) 7)
;(count-pairs inf-pc)  ; Uncomment to verify no return

(newline)


; Ex. 3.17.
; Correct pair counting

(define (count-pairs x) 
  (let ((pairs-found '()))
    (define (add-to-found p)
      (set! pairs-found (cons p pairs-found))
      )
    
    (define (traverse-pairs p)
      (if (and (pair? p) (not (memq p pairs-found)))
          (begin
            (add-to-found p)
            (traverse-pairs (car p))
            (traverse-pairs (cdr p))
            )
          )
      )
    (traverse-pairs x)
    (length pairs-found)
    )
  )

; Testing
(displayln "Testing correct pair-counter.")
; Returns 3 for all 
(check-eqv? (count-pairs three-pc) 3)
(check-eqv? (count-pairs four-pc) 3)
(check-eqv? (count-pairs seven-pc) 3)
(check-eqv? (count-pairs inf-pc) 3)

(newline)


;Ex. 3.18.
; Cycle-finding

(define (is-cycle? li)
  (let ((cdrs-found '()))
    (define (add-to-found p)
      (set! cdrs-found (cons p cdrs-found))
      )
    
    (define (run-list lis)
      (cond 
        ((null? lis) #f )
        ((memq (cdr lis) cdrs-found) #t )
        (else
         (add-to-found lis)
         (run-list (cdr lis))
         )
        )
      )
    (run-list li)
    )
  )

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
  (let ((results (map (lambda (e o) (eqv? e (is-cycle? o)) )
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


; Check the current item in the list against a number of 'forward' items. We limit the look-ahead by tracking the current possible cycle length (using count in run-list). The length is equal to the number of steps in the list that we have 'safely' advanced through.  Since any cycle must return to one of those points in the list, if we advance at least that many steps, at some point the count will be high enough that we will eventually meet the point we started from (if a cycle exists).


(define (is-cycle? li)
  
  ; Returns true if match is found in a-li within count steps
  (define (advance-list a-li match count)
    (cond 
      ((or (= count 0) (null? a-li)) #f )
      ((eq? (cdr a-li) match) #t )
      (else
       (advance-list (cdr a-li) match (- count 1))
       )
      )
    )
  
  (define (run-list lib count)
    (if (null? lib)
        #f
        (if (advance-list lib lib count)
            #t
            (run-list (cdr lib) (+ count 1))
            )
        )
    )
  
  (run-list li 0)
  )

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

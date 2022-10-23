; Section 3.3.2


;; In DrRacket R5RS (or any Scheme needing 'error' defined),
;; this section must be uncommented **

;; Taken from a Stack Overflow response
;; Errors thrown in this way might not halt the instruction sequence

;; create a binding for error
;(define error #f)

;; capture toplevel continuation
;;  assign a function to error, allowing a variable number of arguments to
;;  be passed
;(call-with-current-continuation (lambda (k)
;              (set! error
;                (lambda error-arguments
;                  (display "ERROR :")
;                  (display k)
;                  (for-each (lambda (arg) 
;                              (display " ")
;                              (write arg)
;                              )
;                            error-arguments)
;                  (newline)
;                  )
;                )
;              'error-defined
;             )
;     )
;
;; ** End of error routine 

; Convenience definitions
;
(define true #t)
(define false (not true))

(define (displayln m)
  (display m)
  (newline)
  )


; Queue definitions
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue)) 
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue))) 

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))
      )
  )

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else 
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue
           )
          )
    )
  )

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)
         )
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue
         )
        )
  )

; Ex 3.21.
; Testing queue operations

(displayln "Verifying Queue operation results")
(define q1 (make-queue))

(insert-queue! q1 'a)
; Expect ((a) a)
(insert-queue! q1 'b)
; Expect ((a b) b)
(delete-queue! q1)
; Expect ((b) b)
(delete-queue! q1)
; Expect (() b)


;(define (print-queue <??>)
 

; Testing
(displayln "Verifying correct display of Queue operations with print-queue")

(define q1 (make-queue)) 
(print-queue (insert-queue! q1 'a))
(newline)
(print-queue (insert-queue! q1 'b))
(newline)
(print-queue (delete-queue! q1))
(newline)
(print-queue (delete-queue! q1))
(newline)

(newline)
(displayln "Further verification of print-queue")

(define (print-queue-ops )
  (let ((q1 (make-queue))
        (q2 (insert-queue! (insert-queue! (make-queue) 'c) 'd))
       )
    (display "New (empty) queue: ")
    (print-queue q1)
    (newline)
    (insert-queue! q1 'a)
    (insert-queue! q1 'b)

    (display "Queue with 'a and 'b inserted: ")
    (print-queue q1)
    (newline)

    (display "Front of queue: ")
    (display (front-queue q1))
    (newline)

    (insert-queue! q1 '(d e f)) 
    (display "Queue with a list inserted: ")
    (print-queue q1)
    (newline)

    (display "Queue in a Queue (not expected to work): ")
    (print-queue (insert-queue! q1 q2))
    (newline)
    )
  )

(print-queue-ops)
(newline)

; Ex 3.22
; Queue built as a procedure

(define (make-queue)
  (let ((front-ptr <??>)
        (rear-ptr <??>)
       )
    
    <?definitions of internal procedures?>
    
    (define (dispatch m)
      <??>
    dispatch
    )
  
  )

; Implement queue operations (same names are required for testing with print-queue-ops)



; Testing Queue operation

(displayln "Verifying procedure-based queue.")
(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(newline)
(print-queue (insert-queue! q1 'b))
(newline)
(print-queue (delete-queue! q1))
(newline)
(print-queue (delete-queue! q1))
(newline)

(print-queue-ops)
(newline)

; Ex 3.23.
; Deques

; <? Implement deques ?>


; Testing
(newline)
(displayln "Testing deque")

; Testing functions

(define (test-deque-ends deque expected message)
  (let ((head (front-deque deque))
        (tail (rear-deque deque))
        )
    (if (and (eq? (car expected) head) (eq? (cdr expected) tail))
          (display "passed : ")
          (begin
            (display "FAILED,  expected ")
            (display head)
            (display " = ")
            (display (car expected))
            (display " and ")
            (display tail)
            (display " = ")
            (display (cdr expected))
            (display " : ")
            )
          )
    (display message)
    (newline)
    )
  )       

(define (test-deque-empty deque expected message)

  (define (report-passing)
    (display "passed : ")
    )
  
  (define (report-failing)
    (display "FAILED, Deque is ")
    (if expected
        (display " not ")
        )
    (display " empty :")
   )

  (if (empty-deque? deque)
      (if expected
          (report-passing)
          (report-failing)
          )
      (if expected
          (report-failing)
          (report-passing)
          )
      )
  (display message)
  (newline)
  )

; Test sequence

(define d1 (make-deque))
;(displayln d1)

(test-deque-empty d1 true "Newly-created deque")

; Single entry into empty deque
(begin
  (front-insert-deque! d1 'b) ; (b)
  'done-front-insert
)

(test-deque-ends d1 (cons 'b 'b) "Insertion of one item")

; Add at front or rear
(begin
  (front-insert-deque! d1 'a) ; (a b)
  (rear-insert-deque! d1 'c)  ; (a b c)
  'done-two-insertions
 )

(test-deque-ends d1 (cons 'a 'c) "Two more insertions")
(test-deque-empty d1 false "Three items")

; Delete from front
(begin 
  (front-delete-deque! d1)    ; (b c)
  (rear-insert-deque! d1 'd)  ; (b c d)
  'done-front-delete-rear-insertion
)

(test-deque-ends d1 (cons 'b 'd) "Deletion and rear insertion")

; Delete from rear
(begin
  (rear-delete-deque! d1)     ; (b c)
  (rear-delete-deque! d1)     ; (b)
  (rear-delete-deque! d1)     ; ()
  'done-empty-from-rear
)

(test-deque-empty d1 true "Removal of all entries using rear-delete")

; Insert into empty deque from rear
(begin
  (rear-insert-deque! d1 'x)
  'done-rear-insertion-after-emptying
  )

(test-deque-ends d1 (cons 'x 'x) "Insertion of single value after emptying")
(test-deque-empty d1 false "One item (rear-insertion)")

(begin
  (rear-insert-deque! d1 'y)  ; (x y)
  (front-insert-deque! d1 'w) ; (w x y)
  (rear-insert-deque! d1 'z)  ; (w x y z)
  'done-three-insertions
  )

(test-deque-ends d1 (cons 'w 'z) "Insertions from both front and rear")

; Remove at both ends
(begin 
  (front-delete-deque! d1)    ; (x y z)
  (rear-delete-deque! d1)     ; (x y)
  (front-delete-deque! d1)    ; (y)
  'done-three-deletions
 )

(test-deque-ends d1 (cons 'y 'y) "Deletions to single element")

(begin
  (front-delete-deque! d1)    ; ()
  'done-empty-from-front
)

(test-deque-empty d1 true "All items removed (front)")

(begin 
  (front-insert-deque! d1 'm) ; (m)
  'done-front-insertion-after-emptying
  )

(test-deque-ends d1 (cons 'm 'm) "Front insertion after emptied")
(test-deque-empty d1 false "One item (after empty)")

(begin 
  (rear-delete-deque! d1)     ; ()
  'done-rear-deletion-to-empty
  )

(test-deque-empty d1 true "All items removed (rear)")

; Check what happens with an empty queue - enable individually
;(front-deque d1)
;(rear-deque d1)
;(front-delete-deque! d1)
;(rear-delete-deque! d1)

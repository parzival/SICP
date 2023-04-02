(require ffi/unsafe/atomic)
(require racket/mpair)

; Specially defined for Racket using ffi:
(define (test-and-set! cell)
  (call-as-atomic
   (lambda () 
     (if (mcar cell)
         true
         (begin 
           (set-mcar! cell true)
           false
           )
         )
     )
   )
  )

; This uses map, returning a list of the "thread descriptors"
(define (parallel-execute . procs)
  (map-orig (lambda (p) (thread p)) procs)
  )

; This does not proceed until all the passed threads (in the form of thread-descriptors) are finished 
(define (wait-until-finished td-list)
  (if (not (null? td-list))
      (wait-until-finished (filter thread-running? td-list))
      )
  )

; This will wait but time out and kill the threads after some limit is reached
(define (wait-with-timeout td-list timeout)
  (define (countdown)
    (sleep timeout)
    (displayln "WARNING: Timed out - killing threads")
    )
  
  (define (monitor-threads thds monitor)
    (cond ((null? thds) (kill-thread monitor))
          ((thread-dead? monitor) (for-each-orig kill-thread thds))
          (else
           (monitor-threads (filter thread-running? thds) monitor)
           )
          )
    )
  
  (monitor-threads td-list (thread countdown))
  )


; Procedures from SICP (modified to work in Racket)

(define (make-mutex)
  (let ((cell (mcons false '())))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire) ; retry
                 )
             ) 
            ((eq? m 'release) 
             (clear! cell)
             )
            )
      )
    the-mutex
    )
  )

(define (clear! cell)
  (set-mcar! cell false)
  )

(define (list x . y)
  (mcons x (list->mlist y))
  )

(define (set-cdr! a b) (set-mcdr! a b))
(define (set-car! a b) (set-mcar! a b))

(define (cons a b) (mcons a b))
(define (car l) (mcar l))
(define (cdr l) (mcdr l))
(define (append l1 l2) (mappend l1 l2))

(define memq-orig memq)
(define (memq el li) (memq-orig el (mlist->list li)))

(define map-orig map)
(define (map proc li)
  (mmap proc li)
  )

(define for-each-orig for-each)
(define (for-each . args)
  (void (apply mmap args)) 
  )

(define (length l) (mlength l))

; accumulate & flatmap, from 2.2.3
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (mcar sequence)
          (accumulate op initial (mcdr sequence))
          )
      )
  )

(define (flatmap proc seq)
  (accumulate mappend '() (mmap proc seq))
  )



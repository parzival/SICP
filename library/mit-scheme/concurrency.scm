; Define a few procedures to be more compatible to Racket file

(define displayln write-line)

; convert to milliseconds, round off
(define (sleep secs)
  (sleep-current-thread (floor->exact (* 1000 secs))) 
  )

(define null '())
(define (void . args) 'void )
  
; Only for older MIT-Scheme versions; not needed if using version 10 or later
;(define without-interruption without-interrupts)
;

; These two adapted from
; https://mitpress.mit.edu/sites/default/files/sicp/psets/ps7/parallel.scm

(define (kill-thread thread)
  (let ((event
         (lambda ()
           (exit-current-thread 'RIP))
         )
        )
    (without-interruption 
     (lambda ()
       (case (thread-execution-state thread)
         ('STOPPED (restart-thread thread #t event))
         ('DEAD unspecific)
         (else (signal-thread-event thread event)
               )
         )
       )
     )
    )
  )

(define (parallel-execute . procs)
  (let ((thread-descriptors '()))
    (without-interruption  
     (lambda ()
       (set! thread-descriptors
             (map (lambda (thunk)
                    (let ((thread (create-thread #f thunk)))
                      (detach-thread thread)
                      thread
                      )
                    )
                  procs
                  )
             )
       unspecific
       )
     )
    thread-descriptors
    )
  )


; Thread routines

(define (thread-dead? thd)
  (eq? (thread-execution-state thd) 'DEAD)
  )

; This does not proceed until all the passed threads (in the form of thread-descriptors) are finished 
(define (wait-until-finished td-list)
  (if (not (every thread-dead? td-list))
      (wait-until-finished td-list)
      )
  )

; This will wait but time out and kill the threads after some limit is reached
(define (wait-with-timeout td-list timeout)
  (define (countdown)
    (sleep timeout)
    (display "WARNING: Timed out - killing threads")
    (newline)
    )
  
  (define (monitor-threads thds monitor)
    (cond ((every thread-dead? thds) (kill-thread monitor))
          ((thread-dead? monitor)
           (without-interruption  
            (lambda ()
              (for-each kill-thread thds)
              )
            )
           )
          (else
           (monitor-threads thds monitor)
           )
          )
    )
  
  (monitor-threads td-list (create-thread #f countdown))
  )

; Procedures from SICP

(define (test-and-set! cell)
  (without-interruption
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false
                )
         )
     )
   )
  )


(define (make-mutex)
  (let ((cell (cons false '())))
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
  (set-car! cell false)
  )

; Using accumulate & flatmap, from 2.2.3
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))
          )
      )
  )

(define (flatmap proc seq)
  (accumulate append '() (map proc seq))
  )



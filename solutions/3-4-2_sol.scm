; Choose the concurrency routines as needed for your interpreter:
;(load "library/racket/concurrency.rkt") ; For Racket (Pretty Big)

; For MIT/GNU Scheme
;(load "library/mit-scheme/redefines.scm") ; The redefinition file can only be loaded once per session; comment this out after the first run or load it separately before executing the solution file.
(load "library/mit-scheme/concurrency.scm") ; MIT/GNU Scheme Concurrency

; From text, requires 'make-mutex' to exist
(define (make-serializer)
  (let ((the-mutex (make-mutex)))
    (lambda (p) (define (serialized-p . args)
                  (the-mutex 'acquire)
                  (let ((val (apply p args)))
                    (the-mutex 'release)
                    val
                    )
                  )
      serialized-p
      )
    )
  )

; Serialize the output so that it appears distinctly for each thread
(define output-serializer (make-serializer))

(define protected-display
  (output-serializer display)
  )

(define protected-displayln
  (output-serializer (lambda (m) (display m) (newline)))
  )


; define if necessary
;(define (displayln m)
;  (display m)
;  (newline)
;  )

; Demo of Parallel execution
; Use this to verify that the baseline concurrency routines are working.

;(displayln "Demonstrating parallel execution: x = x*x and x = x+1")
;
; To ensure that the thread execution will intermix, each
; step has had random-length sleeps added.
;
(define x 10)

(define (demo-interleave count)
  (if (> count 0)
      (begin
        (wait-until-finished
         (parallel-execute
          (lambda ()
            (sleep (random))
            (let ((x1 x))
              (sleep (random))
              (let ((x2 x))
                (sleep (random))
                (set! x (* x1 x2))
                )
              )
            )
          (lambda ()
            (sleep (random))
            (let ((x1 x))
              (sleep (* 1.6 (random)))
              (set! x (+ x1 1))
              )
            )
          )
         )
        (displayln x)
        (set! x 10)
        (demo-interleave (- count 1))
        )
      )
  )

;(displayln "Running demo of interleaved processes"
;(demo-interleave 20)

; Interleaving simulation (from 3.4.1 solutions)

; Creates a list of lists, with each element of li moved to the start of the list
(define (shuffle-each-to-front li)
 
  (define (iterate-on-list before el after result)
    (if (null? after)
        (append (list (cons el before)) result)
        (iterate-on-list (append before (list el)) (car after) (cdr after)  (append  (list (cons el (append before after))) result))
        )
    )
  
  (if (null? li)
      (list li)
      (iterate-on-list '() (car li) (cdr li) '())
      )
  )

; attach the first element to all interleavings of remaining elements
(define (interleave-with-first-fixed ln)
  (if (< (length ln) 2)
      ln
      (let ((l1 (car ln))
            )
        (map (lambda (shorter-list) (cons (car l1) shorter-list))
             (interleave-n (cons (cdr l1) (cdr ln)))
             )
        )
      )
  )

(define (interleave-n nlists)
  (cond ((null? nlists) '())
        ((null? (car nlists)) (interleave-n (cdr nlists)))
        (else (flatmap interleave-with-first-fixed (shuffle-each-to-front nlists)) )
        )
  )




; Exercise 3.39
; Possibilities using partial serialization

; (define x 10)
;
; (define s (make-serializer))
;
; (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;                  (s (lambda () (set! x (+ x 1)))))

; All of P1 first (100), then P2 => 101
; All of P2 first (11), then P1 => 121
; P1 squares, then P2, then P1 sets => 100

(define p1-x 0)
(define p2-x 0)
(define x 10)

(define (do-p1a)
  (set! p1-x (* x x))
  )

(define (do-p1b)
  (set! x p1-x)
  )

(define (do-p2)
  (set! x (+ x 1))
  )

(define (process-x-transactions transaction-sequence)
  (set! x 10)
  (for-each (lambda (t) (apply t '())) transaction-sequence)
  x
  )

(displayln "Possible results with partial serialization (squaring/summing)")
(map process-x-transactions (interleave-n (list (list do-p1a do-p1b) (list do-p2))))


; Ex 3.40
; More interleaving possibilities

; (define x 10)
;
; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))

; Alternately:
;
; (define x 10)
;
; (define s (make-serializer))
;
; (parallel-execute (s (lambda () (set! x (* x x))) )
;                   (s (lambda () (set! x (* x x x)) ))
;
; More interleaving
;(parallel-execute (lambda () (set! x (* x x)))
;                  (lambda () (set! x (* x x x))))

; P1 - two reads, one write
(define p1-xa 0)

(define (do-p1a)
  (set! p1-xa x)
  )

(define (do-p1b)
  (set! p1-x (* x p1-xa))
  )

(define (do-p1c)
  (set! x p1-x)
  )

; P2 - three reads, one write
(define p2-xa 0)
(define p2-xb 0)

(define (do-p2a)
  (set! p2-xa x)
  )

(define (do-p2b)
  (set! p2-xb x)
  )

(define (do-p2c)
  (set! p2-x (* x p2-xa p2-xb))
  )

(define (do-p2d)
  (set! x p2-x)
  )

(define (show-results-with-name results names)
  (for-each (lambda (r n) (display n) (display " => ") (display r) (newline)) results names)
  )

(displayln "Possible results with interleaving (repeated multiplication)")
(show-results-with-name
 (map process-x-transactions (interleave-n (list (list do-p1a do-p1b do-p1c) (list do-p2a do-p2b do-p2c do-p2d))))
 (interleave-n (list (list 'p1a 'p1b 'p1c) (list 'p2a 'p2b 'p2c 'p2d)))
 )


; Ex 3.41

;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance
;               )
;        "Insufficient funds"
;        )
;    )
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance
;    )
;  (let ((protected (make-serializer)))
;    (define (dispatch m)
;      (cond ((eq? m 'withdraw) (protected withdraw))
;            ((eq? m 'deposit) (protected deposit))
;            ((eq? m 'balance) ((protected (lambda () balance)))) ; serialized
;            (else (error "Unknown request -- MAKE-ACCOUNT" m))
;            )
;      )
;    dispatch
;    )
;)
; Makes no particular difference to this particular case, since there are no 'intermediate' states for the balance to be in, where reading it would result in an incorrect value, assuming that set! is atomic with respect to the balance variable. The assumption of set! being atomic is reasonable in just about any electronic computer running a Scheme interpreter.

; Ex 3.42

;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance)
;        "Insufficient funds"
;        )
;    )
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance
;    )
;  (let ((protected (make-serializer)))
;    (let ((protected-withdraw (protected withdraw))
;          (protected-deposit (protected deposit))
;          )
;      (define (dispatch m)
;        (cond ((eq? m 'withdraw) protected-withdraw)
;              ((eq? m 'deposit) protected-deposit)
;              ((eq? m 'balance) balance)
;              (else (error "Unknown request -- MAKE-ACCOUNT" m))
;              )
;        )
;      dispatch
;      )
;    )
;  )
; Since of the set of serialized procedures, only one can be run at any given time, there isn't any way for this to cause changes. The serializer is still the same. In the original case, it will have a large number of serialized procedures created in it each time it is accessed (possibly a memory leak?) In the modified version, only two will exist in the set, but the same amount of concurrency is allowed.

; Ex 3.43

; Each exchange will result in some reordering of the amounts, but as long as the exchange completes in series, the result will never alter the amount in each account.  Using the 'original' exchange (serialized per account), when an amount is withdrawn, it is preserved in local memory, and then added to the other account. That may not reflect the actual difference in the accounts, but the money withdrawn from one is always added somewhere else.  Without any transactional serialization, there is no guarantee that an amount added to any given account will actually be reflected in the balance if other transactions run concurrently.
;
;(define (make-account-and-serializer balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance
;               )
;        "Insufficient funds"
;        )
;    )
;  
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance
;    )
;
;  (let ((balance-serializer (make-serializer)))
;    (define (dispatch m)
;      (cond ((eq? m 'withdraw) withdraw)
;            ((eq? m 'deposit) deposit)
;            ((eq? m 'balance) balance)
;            ((eq? m 'serializer) balance-serializer)
;            (else (error "Unknown request -- MAKE-ACCOUNT" m))
;            )
;      )
;    dispatch
;    )
;  )
;
;(define (deposit account amount)
;  (let ((s (account 'serializer))
;        (d (account 'deposit)))
;    ((s d) amount)
;    )
;  )
;
;(define (serialized-exchange account1 account2)
;  (let ((serializer1 (account1 'serializer))
;        (serializer2 (account2 'serializer)))
;    ((serializer1 (serializer2 exchange))
;     account1
;     account2
;     )
;    )
;  )

; Ex 3.44

;(define (transfer from-account to-account amount)
;  ((from-account 'withdraw) amount)
;  ((to-account 'deposit) amount))
; Transfers don't require any protection, since they are a 'one step' process with respect to the accounts. The exchange required both reading the current amounts (to calculate the difference) and then modifying them, which must all be done in a single transaction to be done safely.

; Ex 3.45

;(define (make-account-and-serializer balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance
;               )
;        "Insufficient funds"
;        )
;    )
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance
;    )
;  (let ((balance-serializer (make-serializer)))
;    (define (dispatch m)
;      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
;            ((eq? m 'deposit) (balance-serializer deposit))
;            ((eq? m 'balance) balance)
;            ((eq? m 'serializer) balance-serializer)
;            (else (error "Unknown request -- MAKE-ACCOUNT" m))
;            )
;      )
;    dispatch
;    )
;  )
;
;(define (deposit account amount)
; ((account 'deposit) amount)
;  )
; A serialized function must not call another function in the same serialized set.

; Ex 3.46
; Test-and-set! as procedure

; A concurrent process may call test-and-set!, causing the test (if statement) to run before the cell is actually set. Both tests would yield false, and then both would set the cell (not necessarily an error), and return from the mutex acquire procedure, indicating that the resource is available (thus available to both).


; Ex 3.47
; Semaphores

; Give implementations of a semaphore (allows up to N processes to acquire it) in terms of ...

; a. ...mutexes.

(define (make-semaphore-mutex n)
  (let ((sem-mutex (make-mutex))
        (free-count n)
        )
        (define (retry)
            (sem-mutex 'release)
            (sleep 0.1)
            (the-semaphore 'acquire)
          )
    (define (the-semaphore msg)
      (cond ((eq? msg 'acquire)
             (sem-mutex 'acquire)
             (if (= 0 free-count)
                 (retry)
                 (set! free-count (- free-count 1))
                 )
             (sem-mutex 'release)
             )
            ((eq? msg 'release)
             (sem-mutex 'acquire)
             (if (< free-count n)
                 (set! free-count (+ free-count 1))
                 )
             (sem-mutex 'release)
             )
            (else
             (error "Undefined operation for semaphore : " msg)
             )
            )
      )
    the-semaphore
    )
  )

; b. ...atomic test-and-set!.

(define (make-ring-of-n n gen)
  (let ((tail (list (gen)))
        )

    (define (iter-ring count cur-list)
      (cond ((= count 0) '())
            ((= count 1) (set-cdr! tail cur-list))
            (else
             (iter-ring (- count 1) (cons (gen) cur-list))
             )
            )
      )

    (iter-ring n tail)
    tail
    )
  )

(define (make-semaphore-atomic n)
  (let ((cells (make-ring-of-n n (lambda () (list false)) ))
        (release-lock (list false))  ; Ensures that only one cell is released at a time.
        )

    ; This will keep circling until a there is an unset cell in the buffer.
    (define (acquire-first-available li)
      (if (null? li)
          (error "Circular semaphore ring has terminated list: Cannot continue")
          (if (test-and-set! (car li))
              (acquire-first-available (cdr li))
              true
              )
          )
      )

    ; Release any cell in the buffer if it is set. This will keep circling until there is something to release.
    (define (release-next-available li)
      (if (null? li)
          (error "Circular semaphore ring has terminated list: Cannot continue")
          (if (car (car li))
              (clear! (car li))
              (release-next-available (cdr li))
              )
          )
      )
    
    (define (the-semaphore msg)
      (cond ((eq? msg 'acquire) (acquire-first-available cells))
            ((eq? msg 'release)
             (if (test-and-set! release-lock)
                 (the-semaphore 'release) ; retry
                 (begin
                   (release-next-available cells)
                   (clear! release-lock)
                   )
                 )
             )
            (else
             (error "Undefined operation for semaphore : " msg)
             )
            )
      )
    the-semaphore
    )
  )


; Testing Semaphores

; Using a limited-size buffer to test

; The buffer is supplied with a limiter which blocks input if the
; buffer is 'full'. Another argument indicates the limiter's size,
; which is used for testing to verify that the limiter is working.
; The semaphore is tested by using it as the limiter.

(define (make-buffer size limiter)
  (let ((buf '())
        (overflow false)
        (bufman (make-mutex))
        )

    ; Wraps most of the functions for buffer access
    (define (protected-do x)
      (let ((result '())
            )
        (bufman 'acquire)
        (set! result (x))
        (bufman 'release)
        result
        )
      )
    
    (define (empty?)
      (null? buf)
      )
    
    (define (read)
      (let ((output '())
            )
        (bufman 'acquire)
        (if (not (empty?))
            (begin
              (set! output (car buf))
              (set! buf (cdr buf))
              (limiter 'release)
              )
            )
        (bufman 'release)
        output
        )
      )
      
    ; Reads entire buffer at once, as a list
    (define (read-out)
      (let ((output '())
            )
        (bufman 'acquire)
        (set! output buf)
        (flush)
        (bufman 'release)
        output
        )
      )
    
    
    (define (full?)
      (= (length buf) size)
      )
    
    (define (overflow?)
      overflow
      )
    
    (define (clear-overflow)
      (set! overflow false)
      )

    (define (release-limiter n)
      (if (> n 0)
          (begin
            (limiter 'release)
            (release-limiter (- n 1))
            )
          )
      )
          
          
    (define (flush)
      (release-limiter (length buf))
      (set! buf '())
      )
    
    (define (write item)
      (limiter 'acquire)
      (bufman 'acquire)
      ; If the limiter is working, there should be no overflow
      (if (< (length buf) size)
          (set! buf (cons item buf))
          (set! overflow true)
          )
      (bufman 'release)
      )
    
    (define (dispatch m)
      (cond ((eq? m 'flush) (protected-do flush))
            ((eq? m 'read) (read))
            ((eq? m 'read-out) (read-out))
            ((eq? m 'write) write)
            ((eq? m 'empty?) (protected-do empty?))
            ((eq? m 'overflow?) (protected-do overflow?))
            ((eq? m 'clear-overflow) (protected-do clear-overflow))
            ((eq? m 'full?) (protected-do full?))
            (else (error "Undefined operation for BUFFER " m))
            )
      )
    dispatch
    )
  )

; Buffer interface functions
(define (write-buffer buf item) ((buf 'write) item))

(define (read-buffer buf) (buf 'read))

; Outputs all values in the buffer; a delay argument is used
; so that writes that may be 'waiting' can finish.
(define (print-until-clear buf delay)
  (protected-display " | ")
  (protected-display (buf 'read))
  (sleep delay)
  (if (not (buf 'empty?))
      (print-until-clear buf delay)
      (newline)
      )
  )

; Outputs the entire current contents of the buffer (without waiting
; for pending writes)
(define (print-whole-buffer buf)
  (protected-display (buf 'read-out))
  (newline)
  )

; Verification sequence for buffers

(define (buffer-check-sequence buf)
  ; Write, then read
  (write-buffer buf 1)
  (display "After write, buffer has : ")
  (display (read-buffer buf))
  (newline)
  ; Write & read multiple times, only show last item
  (write-buffer buf 2)
  (read-buffer buf)
  (write-buffer buf 3)
  (display "After two writes & reads, buffer has : ")
  (display (read-buffer buf))
  (newline)
  ; Parallel writes
  (parallel-execute (lambda () (write-buffer buf 1))
                    (lambda () (write-buffer buf 2))
                    (lambda () (write-buffer buf 3))
                    (lambda () (write-buffer buf 4))
                    (lambda () (write-buffer buf 5))
                    (lambda () (write-buffer buf 6))
                    )
  (sleep 0.1)
  (display "Now complete buffer is : ")
  (print-whole-buffer buf)
  (display "Overflow status : ")
  (display (buf 'overflow?))
  (buf 'clear-overflow)
  (newline)
  (display "Overflow status after being cleared :")
  (display (buf 'overflow?))
  (newline)
  (display "Remaining buffer values :")
  (print-until-clear buf 0.1)
  )

(newline)
(displayln "Verifying buffer (no limiter, overflow expected)")
; Testing buffer with a null limiter (overflow expected)
(buffer-check-sequence (make-buffer 5 (lambda (x) '() ) ))

(newline)
(displayln "Verifying buffer (using mutex as limiter, only one item allowed)")
; Testing buffer using mutex (no overflow)
(buffer-check-sequence (make-buffer 1 (make-mutex)))


; Test processes for semaphore

; These create a process that will repeatedly
; write their specified value to the buffer,
; with a random wait time between each write.
;
; They will run forever, but can be stopped by setting
; the global variable *end-tests* to either the datum given
; for writing (to stop just that writer), or to "true",
; which will stop all writers (after the next cycle of writes).

; Note that this does not kill the test writer if there are
; bugs that prevent it from writing to the buffer or the
; process locks up some other way. Setting the variable is
; simply the normal way to conclude a test.

(define (make-test-bufwriter datum buf)
  (define (run)
    (if (or (eq? true end-tests) (eq? datum end-tests))
        'done ; end
        (begin
          (write-buffer buf datum)
          (sleep (+ 0.1 (/ (random) 5.0)))
          (run)
          )
        )
    )
  run
  )

; Running tests concurrently
(define (test-control test-trials tproc-completed? when-trial-done when-all-done)
  (define (test-iter count)
    (if (tproc-completed?)
        ; Repeat unless we have done enough tests
        (if (> count 0)
            (begin
              (when-trial-done)
              (test-iter (- count 1))
              )
            (begin
              (set! end-tests true) ; stop the buffer writers
              (when-all-done) ; run any clean-up necessary
              'test-done ; finished
              )
            )
        ; Test not completed, run again with a wait (avoids blocking the tests)
        (begin
          (sleep 0.05)
          (test-iter count)
          )
        )
    )
  (test-iter test-trials)
  )

(define (test-control-overflow tstore)
  (test-control overflow-trials
                (lambda ()   ; test if trial is done
                  (tstore 'full?)
                  )
                (lambda ()   ; on completion of each trial
                  (if (tstore 'overflow?)
                      (begin
                        (protected-display "Semaphore failed: Buffer overflow detected!")
                        (newline)
                        (tstore 'clear-overflow)
                        )
                      )
                  (print-whole-buffer tstore)
                  )
                (lambda () (tstore 'flush) )  ; when all done
                )
  )

(define (test-control-read_write tstore)
  (let ((buffer-output '())
        )
    (define (randomly-read)
      (sleep (+ 0.1 (/ (random) 5.0))) ; should be just a little longer than the average buffer write
      (set! buffer-output (read-buffer tstore))
      (not (eq? buffer-output '()))  ; return true if something other than empty list
      )
    (define (report-results)
      (protected-display buffer-output)
      (protected-display " ")
      (if (tstore 'overflow?)
          (begin
            (protected-display "Semaphore failed: Buffer overflow detected!")
            (newline)
            (tstore 'clear-overflow)
            )
          )
      )
    (define (flush-remaining)
      (newline)
      (protected-display "Ending test; remaining buffer contents:")
      (print-until-clear tstore 1.0)
      )
    (test-control rw_trials randomly-read report-results flush-remaining)
    )
  )

; Testing semaphores as limiters
(displayln "Testing semaphore as buffer limiters")

(displayln "Using mutex-based semaphore")
(buffer-check-sequence (make-buffer 5 (make-semaphore-mutex 5)))

(displayln "Using atomic semaphore")
(buffer-check-sequence (make-buffer 5 (make-semaphore-atomic 5)))


(displayln "Running semaphore tests (writing to a buffer)")

; Testing semaphores with buffers

(define buffer-size 4)
(define overflow-trials 10)
(define rw_trials 20)

(define tsem-mutex (make-semaphore-mutex buffer-size))

(define tstore-a (make-buffer buffer-size tsem-mutex))

; Global variable used to stop tests
(define end-tests false)

; Create some test writers
(define (tw_a) (make-test-bufwriter 'a tstore-a))
(define (tw_b) (make-test-bufwriter 'b tstore-a))
(define (tw_c) (make-test-bufwriter 'c tstore-a))
(define (tw_d) (make-test-bufwriter 'd tstore-a))
(define (tw_e) (make-test-bufwriter 'e tstore-a))
(define (tc-over) (test-control-overflow tstore-a))
(define (tc-rw) (test-control-read_write tstore-a))

; Wait-with-timeout is recommended here, as this may lock up if the buffer is not being filled
(displayln "Buffer fill test (mutex semaphores)")
(displayln "Expected output is sequence of the filled buffer (list), with no overflow reported")
(wait-with-timeout (parallel-execute (tw_a) (tw_b) (tw_c) (tw_d) (tw_e) tc-over) 20.0)
(newline)

(displayln "Continuous read/write test (may time out)")
(displayln "Expected to get randomized output of letters from all buffers")
(set! end-tests false)
(wait-with-timeout (parallel-execute (tw_a) (tw_b) (tw_c) (tw_d) (tw_e) tc-rw) 20.0)
(newline)

(set! end-tests false)

(define tsem-atomic (make-semaphore-atomic buffer-size))

(define tstore-b (make-buffer buffer-size tsem-atomic))

; Create some test writers
(define (twb_a) (make-test-bufwriter 'a tstore-b))
(define (twb_b) (make-test-bufwriter 'b tstore-b))
(define (twb_c) (make-test-bufwriter 'c tstore-b))
(define (twb_d) (make-test-bufwriter 'd tstore-b))
(define (twb_e) (make-test-bufwriter 'e tstore-b))
(define (tcb-over) (test-control-overflow tstore-b))
(define (tcb-rw) (test-control-read_write tstore-b))

(displayln "Buffer fill test (atomic semaphores)")
(displayln "Expected output is sequence of the filled buffer (list), with no overflow reported")
(wait-with-timeout (parallel-execute (twb_a) (twb_b) (twb_c) (twb_d) (twb_e) tcb-over) 20.0)
(newline)

(displayln "Continuous read/write test (may time out)")
(displayln "Expected to get randomized output of letters from all buffers")
(set! end-tests false)
(wait-with-timeout (parallel-execute (twb_a) (twb_b) (twb_c) (twb_d) (twb_e) tcb-rw) 20.0)
(newline)

; Ex 3.48
; Deadlock avoidance

; Serializer specially redefined with delay to make deadlock more likely
(define (make-serializer)
  (let ((the-mutex (make-mutex)))
    (lambda (p) (define (serialized-p . args)
                  (the-mutex 'acquire)
                  (sleep 0.5)  ; added delay
                  (let ((val (apply p args)))
                    (the-mutex 'release)
                    val
                    )
                  )
      serialized-p
      )
    )
  )


(define account-numbers '())

(define (generate-account-number)
  (let ((new-no (random 1000000)))  ; a better method would generate account numbers in some non-repeating sequence, such as with a LCG.
    (if (memq new-no account-numbers)
        (generate-account-number)
        (begin
          (set! account-numbers (cons new-no account-numbers))
          new-no
          )
        )
    )
  )

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance
               )
        "Insufficient funds"
        )
    )
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
    )
  
  (let ((balance-serializer (make-serializer))
        (account-id (generate-account-number))
        )
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'account-id) account-id)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)
                  )
            )
      )
    dispatch
    )
  )


(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance)
                       )
                    )
        )
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)
    )
  )

; old version
(define (serialized-exchange-old account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        )
    ((serializer1 (serializer2 exchange)) account1 account2)
    )
  )


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        )
    (if (< (account1 'account-id) (account2 'account-id))
        ((serializer2 (serializer1 exchange)) account1 account2)
        ((serializer1 (serializer2 exchange)) account1 account2)
        )
    )
  )

; Testing
(newline)
(displayln "Testing serialized exchange deadlock avoidance")

(define acc1 (make-account-and-serializer 100))
(define acc2 (make-account-and-serializer 50))

(define (tx1) (serialized-exchange acc1 acc2))
(define (tx2) (serialized-exchange acc2 acc1))


; This simply makes sure that regular exchanges still work
(displayln "Serialized Exchange 1 <=> 2 (verifying, no deadlock)")
(displayln "Accounts before exchanging")
(display "Account 1:")
(display (acc1 'balance))
(newline)
(display "Account 2:")
(display (acc2 'balance))
(newline)

(wait-with-timeout (parallel-execute tx1) 10.0)

(displayln "Accounts after exchanging (not valid if exchange timed out):")
(display "Account 1:")
(display (acc1 'balance))
(newline)
(display "Account 2:")
(display (acc2 'balance))
(newline)

(wait-with-timeout (parallel-execute tx2) 10.0)

(newline)
(displayln "Serialized Exchange 1 <=> 2 simultaneously (deadlock possible)")
(displayln "Accounts before exchanging")
(display "Account 1:")
(display (acc1 'balance))
(newline)
(display "Account 2:")
(display (acc2 'balance))
(newline)

(wait-with-timeout (parallel-execute tx1 tx2) 10.0)

(displayln "Accounts after exchanging (not valid if exchange timed out):")
(display "Account 1:")
(display (acc1 'balance))
(newline)
(display "Account 2:")
(display (acc2 'balance))
(newline)


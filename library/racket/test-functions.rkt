; Generic check functions and simple test framework. 

(define (true? x) (not (false? x)))
(define (false? x) (not x))


; All these are checks for FAILURE, which means they return false when the test passes

(define (fails-true? x) (not (true? x)))
(define (fails-false? x) (not (false? x)))

(define (fails-equ? observed expected )
  (if (equ? expected observed)
      false
      (format "FAILURE: ~a is not equ? to ~a." observed expected)
      )
  )

(define (fails-=? observed expected )
  (if (= observed expected)
      false
      (format "Check = FAILED.  Expected ~a but was ~a." expected observed)
      )
  )

(define (fails-equal? observed expected )
  (if (equal? expected observed)
      false
      (format "Equality FAILED. Results were : ~a." observed)
      )
  )

; Fails if all values in a list satisfy some predicate 
; This indicates if any one item fails to satisfy the predicate
(define (fails-any? li pred)
    (not (andmap pred li))
  )

; Test whether something does not cause an error.
; This version is Racket-specific. 
(define (fails-to-error? proc)
    (with-handlers ([exn:fail? exn-expected-handler])
      (proc)
      (format "Error expected but did not occur.") ; No error, failure
      )
  )

; Exception error handler
(define (exn-expected-handler exc)
  false  ; Test passes
  )

; Checks - run a single test (not error-safe)

(define (check-true x) (false? (fails-true? x)))
(define (check-false x) (false? (fails-false? x)))
(define (check-equal expected observed) (false? (fails-equal? observed expected )))
(define (check-= expected observed) (false? (fails-=? observed expected)))
(define (check-equ expected observed) (false? (fails-equ? observed expected)))

; Checks that all pass (if any one fails, then the check fails)
(define (check-all li pred)
  (not (fails-any? li pred))
  )

; 'Tests' - error safe

; During testing, catch all exceptions/errors, but allow for execution to break
(define (catch-all exc)
  (exn:fail? exc)
  )

; The handler displays the error
(define (exc-display exc)
  (display "ERROR: ")
  (display (exn-message exc))
  (newline)
  )

(define (exec-test test-predicate expression-under-test other-args testname)
  (with-handlers 
      ([catch-all exc-display])  ; list of handlers
      (let ((failure (apply test-predicate (cons (expression-under-test) other-args)))
            )
        (if (true? failure)
            (begin
              (display testname)
              (display ": ")
              (if (string? failure)
                  (display failure)
                  (display "FAILED")
                  )
              )
            (display "pass")
            )
        (newline)
        )
      )
    )


; The way these tests work is that they return false if they *pass*.
; This allows the value returned for failure to be a message (see def'n of true? above)


;Tests for truth or falsity
(define (test-true obs-proc . nameargs) 
  (let ((testname (if (null? nameargs) "test-true" (car nameargs)))
        )
  (exec-test fails-true? obs-proc '() testname)
  )
  )

(define (test-false obs-proc . nameargs)
  (let ((testname (if (null? nameargs) "test-false" (car nameargs)))
        )
  (exec-test fails-false? obs-proc '() testname)
  )
  )

; Tests using equality operators
(define (test-equal observed expected . nameargs)
  (let ((testname (if (null? nameargs) "test-equal" (car nameargs)))
        )
  (exec-test fails-equal? observed (list expected) testname)
  )
  )

(define (test-= observed expected . nameargs)
  (let ((testname (if (null? nameargs) "test-=" (car nameargs)))
        )
  (exec-test fails-=? observed (list expected) testname)
  )
  )

(define (test-equ observed expected . nameargs)
  (let ((testname (if (null? nameargs) "test-equ" (car nameargs)))
        )
  (exec-test fails-equ? observed (list expected) testname)
  )
  )
   

; Test that something causes an error (does not use exec-test)
(define (test-for-failure proc . nameargs)
  (let ((testname (if (null? nameargs) "test-for-failure" (car nameargs)))
        )
    (if (fails-to-error? proc)
        (begin
          (display testname)
          (display "FAILED: no error")
          )
        (display "pass")
      )
    )
  )
   
; Not exactly a test, but will signal if an error occurs
(define (test-for-success proc . nameargs)
  (let ((testname (if (null? nameargs) "test-for-success" (car nameargs)))
        )
    (exec-test (lambda (p) false) proc '() testname)
    )
  )



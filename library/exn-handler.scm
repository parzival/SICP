; Syntax rules for R7RS Scheme 

; This doesn't account for errors that occur during handling (typically that leads to an infinite loop).
; Handlers should return the value expected by the continuation.


(define (exception-message exn)
   ((condition-property-accessor 'exn 'message) exn) ; R7RS
)

(define-syntax with-handler
  (syntax-rules ()
    ((with-handler handler thunk)
       (call-with-current-continuation
        (lambda (esc)
          (with-exception-handler (lambda (e) (esc (handler e)) ) thunk)
          )
       )
     )
    )
  )

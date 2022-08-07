; Syntax rules for Racket/R5RS only
(#%require (only racket/base
                 with-handlers
                 exn-message
                 exn:fail?
                 error
                 format
                 time 
                 )
           )
; During testing, catch raised exceptions/errors, but do allow for execution to break

(define exception-message exn-message)

; matcher
(define (catch-all exc)
  (exn:fail? exc)
  )

(define-syntax with-handler
  (syntax-rules ()
    ((with-handler handler thunk)
     (with-handlers
         ((catch-all handler)) ; list of handlers
         (thunk)
       )
     )
    )
  )

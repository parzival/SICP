; Syntax rules for MIT-Scheme

; During testing, catch raised exceptions/errors, but do allow for execution to break

; Handler for possible exceptions
(define (make-handler handler)
  (lambda (maybe-exc)
    (if (condition? maybe-exc)
        (if (condition/error? maybe-exc)
            (handler maybe-exc)
            maybe-exc
            )
        maybe-exc
    )
  )
  )
  

(define-syntax with-handler
  (syntax-rules ()
    ((with-handler handler thunk)
       ((make-handler handler) (ignore-errors thunk))
       )
     )
    )

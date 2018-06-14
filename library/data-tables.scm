; Tag operations
(define (attach-tag type-tag contents)
  (cons type-tag contents)
  )

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)
      )
  )

(define (contents datum)
  (if (pair? datum) 
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)
      )
  )

; Table procedures (from Ch 2-support.scm)
; Modified to use mutable pairs

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false
                  )
              )
            false
            )
        )
      )
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                             (cons (cons key-2 value)
                                    (cdr subtable)
                                    )
                             )
                  )
              )
            (set-cdr! local-table
                       (cons (list key-1
                                     (cons key-2 value)
                                     )
                              (cdr local-table)
                              )
                       )
            )
        )
      'ok
      )    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))
            )
      )
    dispatch
    )
  )

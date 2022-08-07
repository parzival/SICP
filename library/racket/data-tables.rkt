(require racket/mpair)  ; Required for mlist

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
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false
                  )
              )
            false
            )
        )
      )
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)
                                    )
                             )
                  )
              )
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value)
                                     )
                              (mcdr local-table)
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

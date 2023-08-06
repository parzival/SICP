
; Build on previous 
(load-from-lib "streambasic.scm")
(load-from-lib "stream_352.scm")


; From section 3.5.2
; Partial sums
(define (partial-sums s)
  (cons-stream 
   (stream-car s)
   (add-streams (stream-cdr s) (partial-sums s))
   )
  )


; sequence transformation

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1 
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2)) ; Sn+1
        )
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)
                          )
                    ) 
                 (euler-transform (stream-cdr s))
                 )
    )
  )

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))
               )
  )

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)
              )
  )


; Additional stream operations

(define (interleave s1 s2)
  (if (stream-null? s1) 
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))
      )
  )

(define (pairs s t) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t))
    )
   )
  )

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int
                              )
                 )
    )
  int
  )

; Stream Test functions

; This will return the index of the first item to return true for a given predicate;
; A negative return value indicates no element of the stream passes the test
(define (find-first-in-stream test str)
  (define (find-first-in-stream s count)
    (if (stream-null? s)
        -1
        (if (test (stream-car s))
            count
            (find-first-in-stream (stream-cdr s) (+ 1 count))
            )
        )
    )
  (find-first-in-stream str 1)
  )

; This is used to get part of a list.
; a and b are the indexes, and the result is the portion of the list from a to b inclusive
(define (sub-list a b li)
  (cond ((null? li) li)
        ((< b a) '())
        ((= b a) (list (car li)) )
        ((> a 0) (sub-list (- a 1) (- b 1) (cdr li)) )
        ((> b 0) (cons (car li) (sub-list 0 (- b 1) (cdr li))) )
        (else (error "SUB-LIST: Cannot form sub-list from indexes"))
        )
  )

; Compare two streams at the indices given

(define (show-streams-at-points s1 s2 indices)
  (define (show-values-at x)
    (display x)
    (display "      ")
    (display (stream-ref s1 x))
    (display "     ")
    (display (stream-ref s2 x))
    (newline)
    )
  (display " index    s1          s2")
  (newline)
  (for-each show-values-at indices)
  )

; Reduce (infinite) stream by taking every n element of stream
(define (stream-reduce s n)
  (define (get-nth-stream sn indx)
    (if (> indx 0)
        (get-nth-stream (stream-cdr sn) (- indx 1))
        (stream-reduce sn n)
        )
    )
  (cons-stream (stream-car s)
               (get-nth-stream s n)
               )
  )

; Example from text using pi
(define (pi-summands n) 
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))
               )
  )

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4)
  ) 

; Section 2.2.1

(define (header phrase) 
  (newline)
  (display phrase)
  (newline)
  )

; required for Racket/PB
(define nil null) 
(define square sqr)

; Defined this section
(define (length items)
  (define (length-iter a count)
    (if (null? a) count
        (length-iter (cdr a) (+ 1 count))
        )
    )
  (length-iter items 0)
  )

(define (append list1 list2)
  (if (null? list1)
      list2 
      (cons (car list1) (append (cdr list1) list2))
      )
  )

; Ex 2.17
; Getting the last element of a list

(define squares (list 1 4 9 16 25))


; Tests
(header "Testing (last-pair)")
(last-pair squares)
(last-pair (list 2)) 
;(last-pair (list )) ; Invalid input - list cannot be empty


; Ex 2.18
; Reversing a list


; Tests
(header "Testing (reverse)")
(reverse squares)
(reverse (list 1))
(reverse (list ))

; Ex 2.19
; Change counting revisited

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values) 
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)
                 )
              )
        )
  )

; Define the necessary procedures

; Testing
(header "Testing revised change counting")
; These should be true
(= 292 (cc 100 us-coins))
(= 6149 (cc 50 uk-coins))

; Does the list ordering of coin-values matter for these routines?
(= (cc 50 uk-coins) (cc 50 (reverse uk-coins)))

; Ex 2.20
; Procedures with arbitrary numbers of arguments

; Define (same-parity) to accept one or more integers


; Tests
(header "Testing same-parity")
(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; (2 4 6)
(same-parity 4)             
(same-parity 2 43 4 5 6 7 8 4 3 2 2 1)


; Ex 2.21
; Squaring a list

; Complete the definitions

(define (square-list items)
  (if (null? items)
      null
      (cons <??> <??>)
      )
  )

; Tests
(define odds (list 1 3 5 7 9))
(define (test-square-list)
  (displayln (square-list odds))
  (displayln (square-list (list )))
  (displayln (square-list (list 2 0.5 -1)))
  )

(header "Testing square-list (version 1)")
(test-square-list)

(define (square-list items)
  (map <??> <??>) 
  )

(header "Testing square-list (version 2)")
(test-square-list)

; Ex 2.22
; A faulty square-list function

; Explain what is wrong with these implementations

; faulty
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer
                    )
              )
        )
    )
  (iter items nil)
  )

(header "Testing faulty square-list")
(test-square-list)

; also faulty
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))
                    )
              )
        )
    )
  (iter items nil)
  )

(header "Testing second faulty square-list")
(test-square-list)


; Ex 2.23
; Iterating with for-each

; Tests
(header "Testing for-each")

(displayln "Display one element per line:")
(for-each (lambda (x) (display x) (newline))
             (list 57 321 88)
             )

(displayln "Squares displayed after a colon:")
(for-each (lambda (x) (display x) (display ":") (display (square x)) (display " ") )
             (list -5 7 10)
             )
(newline)

; empty list
(displayln "Sending an empty list to for-each:")
(for-each (lambda (x) (display "Should not see this") (display x) (newline))
             (list ))

; list with list element
(displayln "Using a list with different types of data in for-each:")
(for-each (lambda (x) (display x) (newline))
             (list 57 (list "a" "b" "c") 88))

; Using for-each with a testing function
(displayln "Using for-each to indicate different results:")
(for-each (lambda (x) 
            (display x) 
            (display (if (< x 4) " is " " is not "))
            (display "less than 4.")
            (newline)
            )
          (list 2 6 7 -3 0 4 23 9 5 1)
          )

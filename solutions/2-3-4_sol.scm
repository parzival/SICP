; Section 2.3.4

; Setup

(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
  )

(define (leaf? object)
  (eq? (car object) 'leaf)
  )

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
        )
  )

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)
      )
  )

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)
      )
  )

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)
               )
              )
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree)
                    )
              (decode-1 (cdr bits) next-branch)
              )
          )
        )
    )
  (decode-1 bits tree)
  )

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))
        )
  )


; Ex. 2.67.
; Decoding a message using a tree

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)
                                   )
                   )
                  )
  )

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; Decode the message.
(displayln "Decoding sample message")
(decode sample-message sample-tree) ; A D A B B C A

; Ex. 2.68.
; Encoding a message using a tree

(define (encode message tree)
  (if (null? message)
      '() 
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree)
              )
      )
  )



; Write the encode-symbol procedure

(define (encode-symbol sym tree)
  (let ((found-code (find-symbol sym tree)))   
    (if found-code
        found-code
        (error "Unable to encode.  Symbol not found : " sym)
        )
    )
  )

(define (find-symbol sym tree)
  (if (leaf? tree) 
      (if (equal? sym (symbol-leaf tree)) 
          '() ; found symbol here
          false
          )
      (let ((left-found (find-symbol sym (left-branch tree))))
        (if left-found
            (cons 0 left-found)
            (let ((right-found (find-symbol sym (right-branch tree))))
              (if right-found
                  (cons 1 right-found)
                  false
                  )
              )
            )
        )
      )
  )

; Testing

(define decoded-message (decode sample-message sample-tree))

(define (check-true x) (displayln (if x "passed" "failed")))
(define (check-false x) (displayln (if (not x) "passed" "failed")))

(define (check-equal? x y) 
  (displayln (if (equal? x y)
                 "passed"
                 x
                 )
             )
  )

(define single-tree (make-leaf 'A 1))
(define two-sym-tree (make-code-tree (make-leaf 'X 2)
                                     (make-leaf 'Y 1))
  )

(displayln "Testing symbol encoding")
(check-equal? (encode-symbol 'X two-sym-tree) '(0))
(check-equal? (encode-symbol 'Y two-sym-tree) '(1))
(encode-symbol 'A single-tree)  ; might not produce meaningful results
;(encode-symbol 'Z two-sym-tree) ; should return an error

(displayln "Encoding into sample tree")
(check-equal? (encode-symbol 'A sample-tree) '(0))
(check-equal? (encode-symbol 'B sample-tree) '(1 0))
(check-equal? (encode-symbol 'C sample-tree) '(1 1 1))
(check-equal? (encode-symbol 'D sample-tree) '(1 1 0))
;(encode-symbol 'E sample-tree) ; error

(displayln "Testing message encoding")
(check-equal? (encode decoded-message sample-tree) sample-message)

; Ex. 2.69.
; Creating a Huffman code from frequency pairs

; Some procedures defined in the text

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))
                    )
              )
        )
  )

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair)
                               )
                    (make-leaf-set (cdr pairs))
                    )
        )
      )
  )

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
  )

; Define successive-merge 


(define (successive-merge leaf-set)
  (cond
    ((null? leaf-set) '())
    ((null? (cdr leaf-set)) (car leaf-set))
    (else 
     (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                                   (cddr leaf-set)
                                   )
                       )
     )
    )
  )


; Testing
; See next exercise for a bigger test

(newline)
(displayln "Observing Huffman tree generation.")
(generate-huffman-tree '((A 1)))
(generate-huffman-tree '((A 1) (B 1)))
(generate-huffman-tree '((C 2) (A 4) (B 1)))

; Ex. 2.70.
; Coding a message from frequency pairs

; Generate the tree from these pairs:

;A    2  NA   16
;BOOM 1  SHA   3
;GET  2  YIP   9
;JOB  2  WAH   1

(define doo-wop-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define doo-wop-tree (generate-huffman-tree doo-wop-pairs))

; Encode this message

(define silhouettes-message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(displayln "Testing Doo-Wop encoding")
(check-equal? (decode (encode silhouettes-message doo-wop-tree) doo-wop-tree) silhouettes-message)

; Determine the number of bits required 
(display "Using Huffman coding, the message is ")
(display (length (encode silhouettes-message doo-wop-tree)))
(display " bits long.")
(newline)

; Compare to length using a fixed-length code
(display "Using fixed-length coding, the message would be ")
(display (* (length silhouettes-message)
            (inexact->exact (ceiling (/ (log (length doo-wop-pairs)) (log 2))))
            )
         )
(display " bits long.")
(newline)

; Ex. 2.71.
; Unequal relative frequencies

; Because the sum of the set (2^0, 2^1, ... 2^i-1) is smaller than 2^i, the tree will only branch once
; to each leaf, until the bottom two (frequencies 1 and 2) are reached.  There are 2n-1 nodes in 
; the tree: n leaves, and n-1 branching nodes.

; The most frequent symbol has bit length 1, and the least frequent has bit length n-1.

(define (make-bin-list n)
  (define (bin-list-iter partial syms)
    (let ((len (length partial))
          )
      (if (= n len) 
          partial
          (bin-list-iter (cons (list (car syms) (expt 2 len))
                               partial
                               )
                         (cdr syms)
                         )
          )
      )
    )
  (bin-list-iter '() '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
  )


(define bin-5 (generate-huffman-tree (make-bin-list 5)))
(define bin-10 (generate-huffman-tree (make-bin-list 10)))

(length (encode-symbol 'A bin-5))
(length (encode-symbol 'E bin-5))
(length (encode-symbol 'A bin-10))

; Ex. 2.72.

; Each node in the tree (including leaves and branching nodes) is visited at most
; once, and the check only involves one step.  Since the number of nodes in the tree is proportional
; to n, the whole procedure is O(n).

; For the cases presented in 2.71, the steps to encode the most frequent symbol is 2 (which is Theta(1)). 
; The least frequent symbol requires 2n-1 steps (Theta(n)).
; If the symbol frequencies reflect the actual message frequencies, the required time is
; on average n steps.


; Timing test

; Create a version that keeps track of call count

(define (find-symbol sym tree)
  (set! call-count (add1 call-count))
  (if (leaf? tree) 
      (if (equal? sym (symbol-leaf tree)) 
          '() ; found symbol here
          false
          )
      (let ((left-found (find-symbol sym (left-branch tree))))
        (if left-found
            (cons 0 left-found)
            (let ((right-found (find-symbol sym (right-branch tree))))
              (if right-found
                  (cons 1 right-found)
                  false
                  )
              )
            )
        )
      )
  )

(define call-count 0)


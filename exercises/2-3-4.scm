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


; Testing

(define decoded-message ); use result from 2.67

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


; Encode this message

(define silhouettes-message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(displayln "Testing Doo-Wop encoding")
(check-equal? (decode (encode silhouettes-message doo-wop-tree) doo-wop-tree) silhouettes-message)

; Determine the number of bits required 
; Compare to length using a fixed-length code


; Ex. 2.71.
; Unequal relative frequencies

; Determine the number of bits required to encode the most and least frequent symbol in a tree with frequencies equal to powers of 2.

; Ex. 2.72.
; Order of growth for encoding


; See here for documentation : http://planet.plt-scheme.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))



; Preserve predefined functions
(define soegaard-make-segment make-segment)
(define soegaard-segments->painter segments->painter)
(define soegaard-make-vect make-vect)
(define soegaard-make-frame make-frame)
(define soegaard-frame-origin frame-origin)
(define soegaard-frame-edge-1 frame-edge1)
(define soegaard-frame-edge-2 frame-edge2)
(define soegaard-segments->painter segments->painter)

; Possible to do this by using the frame?
(define (draw-line start end)
  (set-canvas 
   (superpose (get-canvas)
              (soegaard-segments->painter 
               (list 
                (soegaard-make-segment 
                 (soegaard-make-vect (xcor-vect start) (ycor-vect start))
                 (soegaard-make-vect (xcor-vect end) (ycor-vect end))
                 )
                )
               )
              )
   )
  )

(define canvas white)
(define (set-canvas c)
  (set! canvas c)
  )


; Paint painter directly, using a frame
(define (paint-with-frame painter frame)
  (set-canvas white)
  (painter frame)
  (paint canvas)
  )

; Turn painter as defined in the exercise into one that works with the library
; This is required since frames defined using one style are incompatible with the other.
(define (painter->soegaard-painter painter frame)
  (let ((pcanvas white))    
    (set-canvas pcanvas)
    (painter frame)
    (set! pcanvas (get-canvas))
    pcanvas
    )
  )

; Defined in text
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller))
        )
      )
  )

(define (corner-split painter n)
  (if (= n 0)
      painter 
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            )
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1)))
              )
          (beside (below painter top-left)
                  (below bottom-right corner)
                  )
          )
        )
      )
  )

; Ex 2.44
; Define the procedure up-split used by corner-split 

; Testing
(displayln "Testing up-split")
(paint (up-split diagonal-shading 2))
(displayln "As used in corner-split")
(paint (corner-split einstein 4))

; Ex 2.45
; Define the generic split operation


; Testing
(displayln "Testing generic split")
(define old-right-split right-split)
(define old-up-split up-split)

(define right-split (split beside below))
(define up-split (split below beside))

(displayln "right")
(paint (right-split diagonal-shading 2))

(displayln "up")
(paint (up-split diagonal-shading 2))

(displayln "corner")
(paint (corner-split einstein 3))


; Ex 2.46
; Define a vector type and vector operations


; This will display the vector in (x,y) format
(define (print-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ",")
  (display (ycor-vect v))
  (display ")")
  )

; Testing
(newline)
(displayln "Testing vectors")

(define v1 (make-vect 0.0 5.0))
(define v2 (make-vect -2.0 3.5))
(displayln "v2 x-coordinate and y-coordinate are:")
(xcor-vect v2)   
(ycor-vect v2)
(display "Same vectors added two ways should be equal:")
(print-vect (add-vect v1 v2)) ; (-2,8.5)
(print-vect (add-vect v2 v1)) ;    "  (vector addition commutes)
(newline)
(display "Swapping the order of subtraction should yield vectors in the opposite direction:")
(print-vect (sub-vect v1 v2)) ; (2.0, 1.5)
(print-vect (sub-vect v2 v1)) ; (-2.0, -1.5)  
(newline)
(display "Scaling vector v2 by 2:")
(print-vect (scale-vect  2.0 v2)) ; (-4, 7)
(newline)
(display "Scaling vector v1 by -0.5:")
(print-vect (scale-vect -0.5 v1)) ; (0, -2.5)  (note possible floating point error for 0.0 * -0.5)
(newline)
(display "Scaling vector v2 by 0:")
(print-vect (scale-vect  0   v2)) ; (0, 0)
(newline)
; A more complex expression
(define v3 (scale-vect 0.5 v2))
; v3 = 1/2 v2
; v2 - v3 + v3  
(display "Result of subtracting and then adding a scaled version of a vector:")
(print-vect (add-vect v3 (sub-vect v2 v3))) ; v2 (-2, 3.5)
(newline)


; Ex 2.47
; Frames defined two ways

; Relies on vector operations of 2.46
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect 
     (origin-frame frame) 
     (add-vect (scale-vect (xcor-vect v)(edge1-frame frame))
               (scale-vect (ycor-vect v)(edge2-frame frame))
               )
     )
    )
  )


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
  )

; define selectors

; Testing version 1
(newline)
(displayln "Testing frame selectors (version 1)")
(define (make-sample-frames)
  (let ((frame1 (make-frame (make-vect 3 1)
                            (make-vect 2.82 1.03)
                            (make-vect -0.68 1.88)
                            )
                )
        )
    (print-vect (origin-frame frame1))
    (print-vect (edge1-frame  frame1))
    (print-vect (edge2-frame  frame1))
    (newline)
    (print-vect (origin-frame frame1))
    (print-vect ((frame-coord-map frame1) (make-vect 0 0)))
    (newline)
    )
  )

(make-sample-frames)
; Version 2
(define (make-frame2 origin edge1 edge2) 
  (cons origin (cons edge1 edge2))
  )

; redefine selectors

; Testing version 2
(displayln "Testing frame selectors (version 2)")
(make-sample-frames)


; Defined in text
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame) (start-segment segment)) 
                           ((frame-coord-map frame) (end-segment segment))
                           )
                )
              segment-list)
    )
  )

; Ex 2.48
; Define a representation for segments
; Based on (segments->painter) it would appear draw-line requires 
; start-segment to return vectors or something treatable as vectors anyway.


; Basic testing
; More usage tests in Ex 2.49
(displayln "Testing segments")

(define test-seg (make-segment 0 1 3 5))
(print-vect (start-segment test-seg))  ; (0,1)
(print-vect (end-segment test-seg))    ; (3,5)

(define test-point (make-segment 0 1 0 1))
(print-vect (start-segment test-point))
(print-vect (end-segment test-point))

; Ex 2.49
; Use segments->painter to define some primitive painters




; Testing
(displayln "Testing primitive painters")

(define def-frame (make-frame (make-vect 0 0) (make-vect 1.0 0) (make-vect 0 1.0)))

(displayln "Frame Outline")
; Bug in painters causes full-size box to go out of frame
(paint-with-frame <?frame outline?> (make-frame (make-vect 0 0) (make-vect 127/128 0) (make-vect 0 127/128)))
(displayln "Cross")
(paint-with-frame <?cross?> def-frame)
(displayln "Diamond")
(paint-with-frame <?diamond?> def-frame)
(displayln "Wave")
(paint-with-frame <?wave?> def-frame)

; With skewed & offset frame
(define skew-frame (make-frame (make-vect 0.1 0.2) (make-vect 0.6 0.3) (make-vect 0.3 0.5))) 
(displayln "Skew outline")
(paint-with-frame <?frame-outline?> skew-frame)
(displayln "Skewed Cross")
(paint-with-frame <?cross?> skew-frame)
(displayln "Skewed Wave")
(paint-with-frame <?wave?> skew-frame)
(displayln "Frame goes outside drawing area")
(paint-with-frame <?cross?> (make-frame (make-vect 0.5 0.6) (make-vect 0.8 -0.4) (make-vect 0.1 0.6)))
(paint-with-frame <?wave?> (make-frame (make-vect 0.1 0.1) (make-vect -0.1 0.6) (make-vect 0.5 0.6)))
) ; Note edge orientation 
; For convenience ahead, redefine these:
(define framebox-painter 
  (painter->soegaard-painter <?frame outline?>
                             (make-frame (make-vect 0 0) 
                                         (make-vect 127/128 0) 
                                         (make-vect 0 127/128)
                                         )
                             )
  )

(define cross-painter (painter->soegaard-painter <?cross?> def-frame))
(define diamond-painter (painter->soegaard-painter <?diamond?> def-frame))
(define sg-wave-painter (painter->soegaard-painter <?wave?> def-frame))

; Painter transformations

;(define (transform-painter painter origin corner1 corner2)
;  (lambda (frame)
;    (let ((m (frame-coord-map frame)))
;      (let ((new-origin (m origin)))
;        (painter (make-frame new-origin
;                             (sub-vect (m corner1) new-origin)
;                             (sub-vect (m corner2) new-origin)
;                             )
;                 )
;        )
;      )
;    )
;  )

; Built-in transform
(define soegaard-transform-painter transform-painter)


; This works thanks to similarity in vector definitions
(define (transform-painter painter origin corner1 corner2)
  ((soegaard-transform-painter origin corner1 corner2) painter)
  )


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0) 
                                         split-point 
                                         (make-vect 0.0 1.0)
                                         )
                      )
          (paint-right (transform-painter painter2
                                          split-point 
                                          (make-vect 1.0 0.0) 
                                          (make-vect 0.5 1.0)
                                          )
                       )
          )
      (lambda (frame) (paint-left frame) (paint-right frame))
      )
    )
  )

(define (flip-vert painter) 
  (transform-painter painter
                     (make-vect 0.0 1.0) 
                     (make-vect 1.0 1.0) 
                     (make-vect 0.0 0.0)
                     )
  )


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0) 
                     (make-vect 0.0 0.0)
                     )
  )

; Ex 2.50
; Define painter transformations to flip horizontally, rotate by 180 and 270

; Testing flippers
(displayln "Verifying painter transforms")
(displayln "flip-vert")
(paint (flip-vert einstein))
(displayln "beside")
(paint (beside sg-wave-painter sg-wave-painter))
(displayln "rotate90")
(paint (rotate90 sg-wave-painter))

(displayln "Testing new transforms")
(displayln "Horizontal flip")
(paint (<?horizontal flip?> sg-wave-painter))
(displayln "Rotate 180")
(paint (<?rotate 180?> einstein))
(displayln "Rotate 270")
(paint (<?rotate 270?> sg-wave-painter))
(displayln "Beside with horizontal flip and 180 rotation:")
(paint (beside sg-wave-painter (<?horizontal flip?> (<?rotate 180?> sg-wave-painter))))


(define (square-limit painter n) 
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (<?horizontal flip?> quarter) quarter))) 
      (below (flip-vert half) half)
      )
    )
  )

; Ex 2.51
; Define below in two ways


; Testing below
(define (below-examples)
  (displayln "wave below cross")
  (displayln (paint (below sg-wave-painter cross-painter)))
  (displayln "diamond beside diamond below wave")
  (displayln (paint (below (beside diamond-painter diamond-painter) sg-wave-painter)))
  )

(displayln "Testing below (version 1)")
(below-examples)

; Now define below in terms of rotations and beside


(displayln "Testing below (version 2)")

(below-examples)

; Ex 2.52

; a. Add a few segments to wave 


; b. Modify corner-split


(displayln "Old corner-split")
(paint-hi-res (corner-split (painter->soegaard-painter <?any painter?> def-frame) 4))

; c. Modify square-limit

(define (square-of-four tl tr bl br) 
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter))) 
          (bottom (beside (bl painter) (br painter)))
          )
      (below bottom top)
      )
    )
  )

(define (square-limit painter n)
  (let ((combine4 (square-of-four <?horizontal flip?>
                                  identity
                                  <?rotate 180?> 
                                  flip-vert)
                  )
        )
    (combine4 (corner-split painter n))
    )
  )

; Testing

(displayln "Old square-limit")
(paint-hi-res (square-limit einstein 5))

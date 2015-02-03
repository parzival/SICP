(require racket/gui/base)  ; Need this to load our graphics

; Set the size of our graphics window
(define frame-size 600) ; Modify this to suit your liking
(define graphics-origin-at-bottom true) ; Set the origin of the drawing area to the lower left, y-axis points up
;  Note: this only applies to functions we define; it has no effect on the existing 'draw-rectangle', etc.
; This means an object will still be drawn from the upper-left corner of the point supplied.


; Set Up Graphics 

; Most of this setup-code is taken from the documentation for the Racket GUI Graphics
; viewable on the web at http://docs.racket-lang.org/gui/drawing-overview.html
; or locally if you have the documentation installed, accessible through the 'Help' menu.


(define picture-window (new frame% [label "Pictures for SICP 2-2-4"]
                            [width frame-size]
                            [height frame-size]))
; Make the drawing area
(define canvas (new canvas% [parent picture-window]
                            [paint-callback (lambda(canvas dc) (draw-picture dc))]  
                            )
  )
                                      
; Get the canvas's drawing context
(define dc (send canvas get-dc))
; Set the smoothing on
(send dc set-smoothing 'aligned)

; Make some pens and brushes
  (define no-pen (make-object pen% "BLACK" 1 'transparent))
  (define no-brush (make-object brush% "BLACK" 'transparent))
  (define blue-brush (make-object brush% "BLUE" 'solid))
  (define yellow-brush (make-object brush% "YELLOW" 'solid))
  (define red-pen (make-object pen% "RED" 2 'solid))
  (define blue-pen (make-object pen% "BLUE" 2 'solid))

; End of graphics set-up

; Example drawing functions 
; This draws two squares with blue lines and yellow interiors, and then a red ellipse under them
(define (draw-example-1 dc)
  (send dc clear)
  (send dc set-pen blue-pen)
  (send dc set-brush yellow-brush)
  (send dc draw-rectangle 100 100 20 20)
  (send dc draw-rectangle 200 100 20 20)
  
  (send dc set-brush no-brush)
  (send dc set-pen red-pen)
  (send dc draw-ellipse 100 (- frame-size 400) 100 50)
  )

; This draws a yellow rectangle with a red border, centered in the frame
(define (draw-example-2 dc)
  (send dc clear)
  (send dc set-pen red-pen)
  (send dc set-brush yellow-brush)
  (let ((x-size 200)
        (y-size 100)
        )
    (send dc draw-rectangle (/ (- frame-size x-size) 2)
                            (/ (- frame-size y-size) 2)
                            x-size
                            y-size
                            )
    )
  )

; Redefine this function for any drawing
(define (draw-picture dc)
  (draw-example-2 dc)
  )

; To show the graphics window with our picture, do this:
(send picture-window show true)

; To show another picture, redefine (draw-picture) and then
; issue it again.  Note that unless (send dc clear) is included 
; in the function, the picture will draw in the same
; area as any existing one.
;
; Example:
; (define draw-picture draw-example-1)
; (draw-picture dc) 

; Slideshow
; This will make a slideshow of all the images in the 
; list 'pics-with-frames' (each a painter-frame list),
; if they are defined as procedures (as in the text and 
; examples below).  Since the segment painters assume
; only one graphics context, this procedure will do the 
; same to prevent confusion.

(define (slideshow pics-with-frames wait-time)
  (for-each (lambda(pic-frame-pair)
              (let ((painter (car pic-frame-pair))
                    (frame   (cadr pic-frame-pair))
                    )
                (send dc clear)
                (send dc set-pen blue-pen)
                (painter frame)
                (sleep/yield wait-time)
                )
              )    
            pics-with-frames)
  )

; This is a helper function to convert a list of 
; painters to all use a single frame.

(define (add-frame-all painter-list frame)
  (map (lambda(painter) (list painter frame)) painter-list)
  )


(define (draw-picture dc)
  (draw-example-2 dc)
  )




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

(define (square-limit painter n) 
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter))) 
      (below (flip-vert half) half)
      )
    )
  )

; Ex 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller))
        )
      )
  )

; Ex 2.45

(define (split copy-dir copy-arrange)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (copy-dir painter (copy-arrange smaller smaller))
          )
        )
    )
  (lambda(painter n) (splitter painter n))
  )

; Testing



; Ex 2.46

(define (make-vect abscissa ordinate)
  (cons abscissa ordinate)
  )

(define (xcor-vect v)
  (car v)
  )

(define (ycor-vect v)
  (cdr v)
  )

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))
             )
  )

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))
             )
  )

(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)
             )
  )

(define (print-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ",")
  (display (ycor-vect v))
  (display ")")
  )

; Testing


(define v1 (make-vect 0.0 5.0))
(define v2 (make-vect -2.0 3.5))
(xcor-vect v2)
(ycor-vect v2)
(print-vect (add-vect v1 v2))
(print-vect (add-vect v2 v1))
(newline)
(print-vect (sub-vect v1 v2))
(print-vect (sub-vect v2 v1))
(newline)
(print-vect (scale-vect  2.0 v2))
(print-vect (scale-vect -0.5 v1))
(print-vect (scale-vect  0   v2))


; Ex 2.47

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame) 
              (add-vect (scale-vect (xcor-vect v)(edge1-frame frame))
                        (scale-vect (ycor-vect v)(edge2-frame frame))
                        )
              )
    )
  )


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
  )

(define (origin-frame f)
  (car f)
  )

(define (edge1-frame f)
  (cadr f)
  )

(define (edge2-frame f)
  (caddr f)
  )

; Testing version 1
(define vi (make-vect 2.82 1.03))
(define vj (make-vect -0.68 1.88))

(define frame1 (make-frame (make-vect 3 1) vi vj))
(print-vect (origin-frame frame1))
(print-vect (edge1-frame  frame1))
(print-vect (edge2-frame  frame1))
(newline)
(print-vect (origin-frame frame1))
(print-vect ((frame-coord-map frame1) (make-vect 0 0)))

(define (make-frame origin edge1 edge2) 
  (cons origin (cons edge1 edge2))
  )

(define (origin-frame f)
  (car f)
  )

(define (edge1-frame f)
  (cadr f)
  )

(define (edge2-frame f)
  (cons (caddr f) (cdddr f))
  )

; Testing version 2

(newline)
(define frame1 (make-frame (make-vect 3 1) vi vj))
(print-vect (origin-frame frame1))
(print-vect (edge1-frame  frame1))
(print-vect (edge2-frame  frame1))
(newline)
(print-vect (origin-frame frame1))
(print-vect ((frame-coord-map frame1) (make-vect 0 0)))



; Ex 2.46

; This will draw lines as expected for these exercises.
; Note we are using dc to refer to the above-defined 
; context.  Unlike the 'draw-example' cases, this only
; works in reference to that single drawing context.

(define (draw-line start end)
  (if graphics-origin-at-bottom     ; This inverts Racket/gui's default direction
                                    ; to match the SICP usage.
      (send dc draw-line  (xcor-vect start)
                          (- frame-size (ycor-vect start))
                          (xcor-vect end)
                          (- frame-size (ycor-vect end))
                          )
      (send dc draw-line  (xcor-vect start)
                          (ycor-vect start)
                          (xcor-vect end)
                          (ycor-vect end)
                          )
      )
  )


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
; Based on (segments->painter) it would appear draw-line requires 
; start-segment to return vectors or something treatable as vectors anyway.

(define (make-segment x1 y1 x2 y2)
  (list (make-vect x1 y1) (make-vect x2 y2))
  )

(define (start-segment seg)
  (car seg)
  )

(define (end-segment seg)
  (cadr seg)
  )


(define (make-segment-polar x0 y0 length angle)
  (list (make-vector x0 y0)
        (make-vector (+ x0 (* length (cos angle))) 
                     (+ y0 (* length (sin angle)))
                     )
        )
  )

; Basic testing
; Usage tests in Ex 2.49

(define test-seg (make-segment 0 1 3 5))
(print-vect (start-segment test-seg))
(print-vect (end-segment test-seg))


; Ex 2.49

(define framebox-painter
  (segments->painter (list 
                      (make-segment 0 0 0 1)
                      (make-segment 0 0 1 0)
                      (make-segment 1 0 1 1)
                      (make-segment 0 1 1 1)
                      )
                     )
  )

(define cross-painter
  (segments->painter (list
                      (make-segment 0 0 1 1)
                      (make-segment 1 0 0 1)
                      )
                     )
  )

(define diamond-painter
  (segments->painter (list
                      (make-segment 0   0.5 0.5 0  )
                      (make-segment 0.5 0   1   0.5)
                      (make-segment 1   0.5 0.5 1  )
                      (make-segment 0   0.5 0.5 1  )
                      )
                     )
  )


(define wave-painter
  (segments->painter (list
                     (make-segment 0.000 0.842 0.147 0.594)
(make-segment 0.147 0.594 0.299 0.647)
(make-segment 0.299 0.647 0.398 0.647)
(make-segment 0.398 0.647 0.345 0.845)
(make-segment 0.345 0.845 0.396 1.000)
(make-segment 0.591 1.000 0.644 0.842)
(make-segment 0.644 0.842 0.594 0.647)
(make-segment 0.594 0.647 0.741 0.647)
(make-segment 0.741 0.647 1.000 0.348)
(make-segment 0.000 0.647 0.150 0.396)
(make-segment 0.150 0.396 0.302 0.578)
(make-segment 0.302 0.578 0.348 0.497)
(make-segment 0.348 0.497 0.246 0.000)
(make-segment 0.398 0.000 0.495 0.291)
(make-segment 0.495 0.291 0.591 0.000)
(make-segment 0.746 0.000 0.594 0.447)
(make-segment 0.594 0.447 1.000 0.144)
                      )
                     )
  )



(define centered-frame (make-frame (make-vect (/ (- frame-size 200) 2) (/ (- frame-size 200) 2)) 
                                   (make-vect 200 0)
                                   (make-vect 0 200)
                                   )
  )
(define tilted-frame (make-frame (make-vect 200 200) (scale-vect 100 vi) (scale-vect 100 vj)))

(define ex-pics (list framebox-painter cross-painter diamond-painter wave-painter)) 

(slideshow (add-frame-all ex-pics centered-frame) 0.8)

;(define (draw-picture dc)
;  (draw-all dc tilted-frame Ex-pics)
;  )

;(draw-picture dc)
                       
    

; Ex 2.50

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)
                             )
                 )
        )
      )
    )
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

(define (flip-horiz painter) 
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)
                     )
  )

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0) 
                     (make-vect 1.0 0.0)
                     )
  )

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0) 
                     (make-vect 1.0 1.0)
                     )
  )

; Testing flippers

(slideshow (add-frame-all (list wave-painter (flip-vert wave-painter) (flip-horiz wave-painter)) centered-frame) 1.5)


; Testing painter rotation

(slideshow (add-frame-all (list wave-painter 
                                (rotate90 wave-painter) 
                                (rotate180 wave-painter) 
                                (rotate270 wave-painter)
                                )
                          centered-frame
                          )
           1.5
           )


; Ex 2.51

(define (below painter1 painter2)
  (let ((split-frac 0.5))
    (let ((paint-lower (transform-painter painter1
                                         (make-vect 0.0 0.0) 
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.0 split-frac)
                                         )
                      )
          (paint-upper (transform-painter painter2
                                          (make-vect 0.0 split-frac)  
                                          (make-vect 1.0 split-frac)
                                          (make-vect 0.0 1.0)
                                          )
                       )
          )
      (lambda (frame) (paint-lower frame) (paint-upper frame))
      )
    )
  )

; Testing beside and below

(slideshow (add-frame-all (list 
                           wave-painter 
                           (beside wave-painter wave-painter) 
                           (below wave-painter wave-painter)
                           (beside diamond-painter cross-painter)
                           (below diamond-painter wave-painter)
                           (beside diamond-painter (below cross-painter cross-painter))
                           )
                          centered-frame
                          )
           1.5
           )



; Now define below in terms of rotations

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2)))
  )

; Testing again
(slideshow (add-frame-all (list 
                           wave-painter 
                           (beside wave-painter wave-painter) 
                           (below wave-painter wave-painter)
                           (beside diamond-painter cross-painter)
                           (below diamond-painter wave-painter)
                           (beside diamond-painter (below cross-painter cross-painter))
                           )
                          centered-frame
                          )
           1.5
           )



; Testing other operations


(define right-split (split beside below))
(define up-split (split below beside))

(slideshow (add-frame-all (list 
                           wave-painter 
                           (right-split wave-painter 4) 
                           (up-split wave-painter 4)
                           (corner-split diamond-painter 8)
                           (square-limit wave-painter 6)
                           )
                          centered-frame
                          )
           1.5
           )



(define (draw-pict dc)
  (send dc clear)
  (Pic centered-frame)
  )


; Ex 2.52

; a. Add a few segments to wave (smile added)

(define smile-painter
  (segments->painter (list
                      (make-segment 0.455 0.832 0.476 0.805)
(make-segment 0.476 0.805 0.497 0.805)
(make-segment 0.497 0.805 0.521 0.834)
                      )
                     )
  )

(define wave-smile-painter
  (lambda(frame) 
    (wave-painter frame)
    (smile-painter frame)
    )
  )

; Testing
(define Pic wave-smile-painter)

(draw-pict dc)

; b. 

(define (corner-split painter n)
  (if (= n 0)
      painter 
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            )
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1)))
              )
          (beside (below painter top-left)
                  (below bottom-right corner)
                  )
          )
        )
      )
  )

(define Pic (corner-split wave-smile-painter 4))
(draw-pict dc)


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
  (let ((combine4 (square-of-four flip-horiz
                                  identity
                                  rotate180 
                                  flip-vert)
                  )
        )
    (combine4 (corner-split painter n))
    )
  )

(define (square-limit-modded painter n)
  (let ((combine4 (square-of-four identity
                                  flip-horiz
                                  flip-vert
                                  rotate180
                                 )
                  )
        )
    (combine4 (corner-split painter n))
    )
  )

; Testing

(define big-frame (make-frame (make-vect (/ (- frame-size 400) 2) (/ (- frame-size 400) 2)) 
                                   (make-vect 400 0)
                                   (make-vect 0 400)
                                   )
  )

(slideshow (add-frame-all (list (square-limit wave-smile-painter 4) (square-limit-modded wave-smile-painter 4)) big-frame) 3)



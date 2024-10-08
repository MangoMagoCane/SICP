#lang sicp

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect xvect yvect)
  (make-vect (+ (xcor-vect xvect)
                (xcor-vect yvect))
             (+ (ycor-vect xvect)
                (ycor-vect yvect))))

(define (sub-vect xvect yvect)
  (make-vect (- (xcor-vect xvect)
                (xcor-vect yvect))
             (- (ycor-vect xvect)
                (ycor-vect yvect))))

(define (scale-vect vect s)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))

(define vec1 (make-vect 1 2))
(define vec2 (make-vect 3 4))

(add-vect vec1 vec2)
(sub-vect vec1 vec2)
(scale-vect vec1 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define frame-1 (make-frame 1 2 3))
(origin-frame frame-1)
(edge1-frame frame-1)
(edge2-frame frame-1)

;_____________________________________________________________________________________________________

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))

(define (edge1-frame-2 frame)
  (cadr frame))

(define (edge2-frame-2 frame)
  (cddr frame))

(define frame2 (make-frame-2 1 2 3))
(origin-frame-2 frame2)
(edge1-frame-2 frame2)
(edge2-frame-2 frame2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-48

(define draw-line "no implementation")
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-segment xvect yvect)
  (cons xvect yvect))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-49

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define bl (make-vect 0 0))
(define br (make-vect 0 1))
(define tl (make-vect 1 0))
(define tr (make-vect 1 1))

(define t (make-vect 0.5 1))
(define b (make-vect 0.5 0))
(define l (make-vect 0 0.5))
(define r (make-vect 1 0.5))

(define painter->frame-outline
  (segments->painter
   (list (make-segment bl br)
         (make-segment br tr)
         (make-segment tr tl)
         (make-segment tl bl))))

(define painter->X
  (segments->painter
   (list (make-segment bl tr)
         (make-segment br tl))))

(define painter->diamond
  (segments->painter
   (list (make-segment b r)
         (make-segment b l)
         (make-segment t r)
         (make-segment t l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-50

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 1.0 1.0)
   (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter
   painter
   (make-vect 1.0 1.0)
   (make-vect 0.0 1.0)
   (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-51

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter2
                              (make-vect 0.0 0.0)
                              (make-vect 0.0 1.0)
                              split-point))
          (paint-top
           (transform-painter painter1
                              split-point
                              (make-vect 0.0 1.0)
                              (make-vect 1.0 0.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-a painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))
#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(require "PhysicsEquations.rkt")
(provide carHitBox)
(provide ptG)
(provide demo)
(provide hitWall)

#|Hit Boxes and How do They Work|#

#|
TODO:
 - Colission with walls
 - Colission with ball
 - Demo calculations
|#

#|
carHitBox = Calculates the hitbox for a car

@param veh = car object
@return    = list of corners of car

(a,b)      (c,d)
  +----------+
  |          |
  +----------+
(g,h)      (e,f)
|#

(define (carHitBox veh)
  (let* ((x (veh 'get-x)) #|car's x coordinate|#
         (y (veh 'get-y))  #|car's y coordinate|#
         (t (degrees->radians (veh 'get-theta)))  #|car's theta|#
         (w2 (/ car-width 2))   #|half the car's width|#
         (L2 (/ car-length 2))  #|half the car's length|#
         (ac (cos (- t pi)))    #|adjacent side length of triangle to edge|#
         (as (sin (- t pi))))   #|opposite side length of triangel to edge|#
    (list (list (+ x (* w2 ac) (- (* L2 as)))
                (+ y (* w2 as) (+ (* L2 ac))))  #|(a,b)|#
          (list (- x (* w2 ac) (- (* L2 as)))
                (+ y (* w2 as) (+ (* L2 ac))))  #|(c,d)|#
          (list (- x (* w2 ac) (- (* L2 as)))
                (- y (* w2 as) (+ (* L2 ac))))  #|(e,f)|#
          (list (+ x (* w2 ac) (- (* L2 as)))
                (- y (* w2 as) (+ (* L2 ac))))) #|(g,h)|#
    ))

#|
colission = checks to seee if two cars have collided

@return = bool true if collided

c1       c2
A     B  G     H
+-----+  +-----+
|     |  |     |
+-----+  +-----+
D     C  F     E
|#

(define (colission)
  (let* ((hb1 (carHitBox car1)) #|HitBox for car1|#
         (hb2 (carHitBox car2)) #|HitBox for car2|#
         (A (car hb1))     #|pt A car1|#
         (B (cadr hb1))    #|pt B car1|#
         (C (caddr hb1))   #|pt C car1|#
         (D (cadddr hb1))  #|pt D car1|#
         (E (car hb2))     #|pt E car2|#
         (F (cadr hb2))    #|pt F car2|#
         (G (caddr hb2))   #|pt G car2|#
         (H (cadddr hb2))) #|pt H car2|#
    #|check to see if one hitbox is inside the other|#
    (cond [(and (ptG A E) (ptG E C)) #t]
          [(and (ptG B F) (ptG F D)) #t]
          [(and (ptG B H) (ptG H D)) #t]
          [(and (ptG A G) (ptG G C)) #t]
          [else (list hb1 hb2)])))

#|
ptG = Greater than for Points

@param p1 = one point
@param p2 = other point
@return   = bool true it p1 > p2
|#

(define (ptG p1 p2)
  (and (< (car p1) (car p2)) (< (cadr p1) (cadr p2))))

#|
demo = determines demolition

c1
A     B
+-----+
|     |
+-----+
D     C
|#

(define (demo)
  (if (colission)
      (let ((c1v (findVelo (car1 'get-velo)))
            (c2v (findVelo (car2 'get-velo))))
        (cond [(> c1v c2v) (car2 'reset)]
              [(> c2v c1v) (car1 'reset)]
              [else (begin (car1 'reset) (car2 'reset))]))
      "no colision"))

#|
hitWall = Determines if the car has hit an edge

@param c = car
@return  = 
|#

(define (hitWall c)
  (let* ((hb (carHitBox c))
           (A (car hb))
           (B (cadr hb))
           (C (caddr hb))
           (D (cadddr hb))
           (vel (c 'get-velo)))
      (cond [(or (< (car A) 0) (< (car B) 0) (< (car C) 0) (< (car D) 0)) #|If the car's x is less that 0|#
             (cond [(or (< (cadr A) 0) (< (cadr B) 0) (< (cadr C) 0) (< (cadr D) 0))  #|and y is less than 0|#
                    (begin ((c 'update-pos) (list 50 50))
                           ((c 'update-velo) (list 0 0)))]
                   [(or (> (cadr A) (getAxis 'y)) (> (cadr B) (getAxis 'y)) (> (cadr C) (getAxis 'y)) (> (cadr D) (getAxis 'y)))
                    (begin ((c 'update-pos) (list 50 (- (getAxis 'y) 50)))            #|and y is greater than max|#
                           ((c 'update-velo) (list 0 0)))]
                   [else (begin ((c 'update-pos) (list 50 (c 'get-y)))
                                ((c 'update-velo) (list 0 (cadr vel))))])]   #|\/ If the car's x is greater than max|#
            [(or (> (car A) (getAxis 'x)) (> (car B) (getAxis 'x)) (> (car C) (getAxis 'x)) (> (car D) (getAxis 'x)))
             (cond [(or (< (cadr A) 0) (< (cadr B) 0) (< (cadr C) 0) (< (cadr D) 0)) #|and y is less than 0|#
                    (begin ((c 'update-pos) (list (- (getAxis 'x) 50) 50))
                           ((c 'update-velo) (list 0 0)))]                           #|\/ and y is greater than max|#
                   [(or (> (cadr A) (getAxis 'y)) (> (cadr B) (getAxis 'y)) (> (cadr C) (getAxis 'y)) (> (cadr D) (getAxis 'y)))
                    (begin ((c 'update-pos) (list (- (getAxis 'x) 50) (- (getAxis 'y) 50)))
                           ((c 'update-velo) (list 0 0)))]
                   [else (begin ((c 'update-pos) (list (- (getAxis 'x) 50) (c 'get-y)))
                                ((c 'update-velo) (list 0 (cadr vel))))])]
            [else
             (cond [(or (< (cadr A) 0) (< (cadr B) 0) (< (cadr C) 0) (< (cadr D) 0))  #|if y is less than 0|#
                    (begin ((c 'update-pos) (list (c 'get-x) 50))
                           ((c 'update-velo) (list (car vel) 0)))]                    #|\/ if y is greater than max|#
                   [(or (> (cadr A) (getAxis 'y)) (> (cadr B) (getAxis 'y)) (> (cadr C) (getAxis 'y)) (> (cadr D) (getAxis 'y)))
                    (begin ((c 'update-pos) (list (c 'get-x) (- (getAxis 'y) 50)))
                           ((c 'update-velo) (list (car vel) 0)))]
                   [else ((c 'update-pos) (list (c 'get-x) (c 'get-y)))])])))

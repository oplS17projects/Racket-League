#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(provide carHitBox)

#|Hit Boxes and How do they Work|#

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

(define (carHitBoxB veh)
  (let ((x (veh 'get-x))
        (y (veh 'get-y))
        (w2 (/ car-width 2))
        (l2 (/ car-length 2)))
    (list (list (- x w2) (- y l2))
          (list (+ x w2) (- y l2))
          (list (+ x w2) (+ y l2))
          (list (- x w2) (+ y l2)))))

(define (carHitBox veh)
  (let* ((x (veh 'get-x)) #|car's x coordinate|#
        (y (veh 'get-y))  #|car's y coordinate|#
        (t (degrees->radians (veh 'get-theta)))  #|car's theta|#
        (w2 (/ car-width 2))  #|half the car's width|#
        (L2 (/ car-length 2)) #|half the car's length|#
        (ac (cos (- t pi)))   #|adjacent side length of triangle to edge|#
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

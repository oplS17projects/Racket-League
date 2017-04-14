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
        (as (sin (- t pi)))   #|opposite side length of triangel to edge|#
        (ap (+ x (* L2 ac)))  #|x-cord of right most length middle|#
        (am (- x (* L2 ac)))  #|x-cord of left most length middle|#
        (bp (+ y (* L2 as)))  #|y-cord of right most length middle|#
        (bm (- y (* L2 as)))) #|y-cord of left most length middle|#
    (list (list (+ am (- L2) (* w2 ac))
                (- bm L2 (* w2 as)))  #|(a,b)|#
          (list (- ap (- L2) (* w2 ac))
                (- bp L2 (* w2 as)))  #|(c,d)|#
          (list (- ap (- L2) (* w2 ac))
                (+ bp L2 (* w2 as)))  #|(e,f)|#
          (list (+ am (- L2) (* w2 ac))
                (+ bm L2 (* w2 as)))) #|(g,h)|#
    ))

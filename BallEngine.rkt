#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(require "PhysicsEngine.rkt")
(provide ballPos)
(provide update)

#|Ball physics.  Similar to car, but bounces off edges|#

#|
ballPos = Set the ball's new position

@param pos = the position of the ball
@param vel = the velocity of the ball
@param rad = the radius of the ball
@return    = List with car position and cdr velocity
|#
(define (ballPos pos vel rad)
  (let* ((nvel (currentvel (car vel) (cadr vel) 75))          #|Sets balls velocity|#
         (nXpos (ballNewPos (car pos) (car nvel) rad 'x))     #|Finds x pos and vel|#
         (nYpos (ballNewPos (cadr pos) (cadr nvel) rad 'y)))  #|Finds y pos and vel|#
    ((ball 'update-ball) (list (list (car nXpos) (car nYpos))
                                   (list (cadr nXpos) (cadr nYpos))))))

#|
ballNewPos = calculates new ball x position

@param pos = position
@param vel = velocity
@param rad = ball radius
@return    = new position and velocity
|#
(define (ballNewPos pos vel rad axis)
  (cond [(>= 0 (- pos rad)) (list (- pos vel) (- vel))]
        [(<= (getAxis axis) (+ pos rad)) (list (- pos vel) (- vel))]
        [else (list (+ pos vel) vel)]))

#|
update = updates positions and velocities

@param lst = list of everything
@return    = updated everything
|#

(define (update w)
  (carPos (car1 'get-pos) (car1 'get-velo))
  (carPos (car2 'get-pos) (car2 'get-velo))
  (ballPos (ball 'get-pos) (ball 'get-velo) 15)
  w)

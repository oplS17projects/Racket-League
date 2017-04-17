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
  (let* ((nvel (list 10 10)) ;;(currentvel (car vel) (cadr vel) 75))
         (nXpos (ballNewPos (car pos) (car nvel) rad 'x))
         (nYpos (ballNewPos (cadr pos) (cadr nvel) rad 'y)))
    ((ball 'update-ball) (list (list 500 375) (list 0 0)))))
     #|(list (list (car nXpos) (car nYpos))
                                   (list (cadr nXpos) (cadr nYpos))))))|#

#|
ballNewPos = calculates new ball x position

@param pos = position
@param vel = velocity
@param rad = ball radius
@return    = new position and velocity
|#
(define (ballNewPos pos vel rad axis)
  (cond [(= 0 (- pos rad)) (list (- pos vel) (- vel))]
        [(and (equal? axis 'x)(= (car winSize) (+ pos rad))) (list (- pos vel) (- vel))]
        [(and (equal? axis 'y)(= (cadr winSize) (+ pos rad))) (list (- pos vel) (- vel))]
        [else (list (+ pos vel) vel)]))

#|
update = updates positions and velocities

@param lst = list of everything
@return    = updated everything
|#

(define (update w)
  ;((car1 'update-car) (carPos (car1 'get-pos) (car1 'get-velo)))
  ;((car2 'update-car) (carPos (car2 'get-pos) (car2 'get-velo)))
  ((ball 'update-ball) (ballPos #|(ball 'get-pos)|#(list 500 375) (list 0.001 0.001) 15))
  w)

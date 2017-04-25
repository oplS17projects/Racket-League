#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(require "PhysicsEquations.rkt")
(provide left-turn)
(provide right-turn)
(provide accelerate-car)
(provide slow-car)
(provide activate-boost)
(provide carPos)
(provide currentvel)
(provide getAxis)

#|Creates new car position (pt mass)|#

#|
carpos = the new position of the car

@param pos = (x,y) position of the car
@param vel = (vx,vy) velocity of the car as components
@return    = returns a list consisting of the new position of the car
             and the new velocity
|#
(define (carPos pos vel)
  (let* ((nvel (currentvel (car vel) (cadr vel) 50))
         (nXpos (newPos (car pos) (car nvel) 'x))
         (nYpos (newPos (cadr pos) (cadr nvel) 'y)))
    (list (list nXpos nYpos) nvel)))

#|
newPos = changes a position based on a velocity

@param p  = position
@param vp = velocity component
@param maxWin = width of axis
@return   = new position
|#
(define (newPos p vp axis)
  (let ((calc (+ p vp)))
    (cond [(or (= 0 calc)(> 0 calc)) 0]
          [(> calc (getAxis axis)) (getAxis axis)]
          [else calc])))

#|
currentvel = calculates the current velocity of the object

@param max = max allowed velocity of object
@param vx  = x velocity
@param vy  = y velocity
@return    = returns a list of vx and vy so the current velocity
             is less than the max value
|#
(define (currentvel vx vy max)
  (let* ((v (findVelo (list vx vy)))
         (c (if (= v 0) 0 (acos (/ vx v)))))
    (cond [(= 0 v) (list 0 0)]
          [(> max v) (list (- vx 0.001) (- vy 0.001))]
          [(> v max)(list (- (* max (cos c)) 0.001)
                      (- (* max (sin c)) 0.001))])))


#|
left-turn = turn the car left
|#

(define (left-turn)
  (begin
      (if (car1 'turning-left?)
          (let ((ntheta (+ (car1 'get-theta) 15)))
            ((car1 'update-car) (list (car1 'get-pos)
                                      (car1 'get-velo)
                                      ntheta)))
          "Nothing to do")
      (if (car2 'turning-left?)
          (let ((ntheta (+ (car2 'get-theta) 15)))
            ((car2 'update-car) (list (car2 'get-pos)
                                      (car2 'get-velo)
                                      ntheta)))
          "Nothing to do")))

#|
right-turn = turn the car right
|#

(define (right-turn)
  (begin
      (if (car1 'turning-right?)
          (let ((ntheta (- (car1 'get-theta) 15)))
            ((car1 'update-car) (list (car1 'get-pos)
                                      (car1 'get-velo)
                                      ntheta)))
          "Nothing to do")
      (if (car2 'turning-right?)
          (let ((ntheta (- (car2 'get-theta) 15)))
            ((car2 'update-car) (list (car2 'get-pos)
                                      (car2 'get-velo)
                                      ntheta)))
          "Nothing to do")))

#|
slow-car = slow the car
|#
(define (slow-car)
  (begin
      (if (car1 'decel?)
          ((car1 'update-car)(list (car1 'get-pos)
                                     (thetaXY (list (car1 'get-theta) (- (findVelo (car1 'get-velo)) 0.2)))
                                     (car1 'get-theta)))
          "Nothing to do")
      (if (car2 'decel?)
          ((car2 'update-car)(list (car2 'get-pos)
                                     (thetaXY (list (car2 'get-theta) (- (findVelo (car2 'get-velo)) 0.2)))
                                     (car2 'get-theta)))
          "Nothing to do")))
       
#|
accelerate-car = accelerate the car
|#

(define (accelerate-car)
  (begin
      (if (car1 'accel?)
          ((car1 'update-car)(list (car1 'get-pos)
                                   (thetaXY (list (car1 'get-theta) (+ (findVelo (car1 'get-velo)) 0.2)))
                                   (car1 'get-theta)))
          "Nothing to do")
      (if (car2 'accel?)
          ((car2 'update-car)(list (car2 'get-pos)
                                   (thetaXY (list (car2 'get-theta) (+ (findVelo (car2 'get-velo)) 0.2)))
                                   (car2 'get-theta)))
          "Nothing to do")))

(define (activate-boost num)
  "Nothing Implemented")

#lang racket

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(require "PhysicsEquations.rkt")
(require "HitBoxes.rkt")
(provide left-turn)
(provide right-turn)
(provide update-car)
(provide slow-car)
(provide activate-boost)
(provide carPos)
(provide currentvel)
(provide getAxis)

#|Creates new car position (pt mass)|#

#|
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

|#

#|
carPos = Finds the new position of the car

@param c = The car to be updated
@return  = List consisting of the new position and velocity of the car
|#

(define (carPos c)
  (let* ((vx (car (c 'get-velo)))
         (vy (cadr (c 'get-velo)))
         (nvel (currentvel vx vy 50))
         (hit (hitWall c)))
    (if (cdr hit) (list (car hit) '(0 0))
      (list (+ (caar hit) vx)
            (+ (cadar hit) vy)))))
           

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

#|
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

|#

(define (activate-boost num)
  "Nothing Implemented")

(define (update-car c)
  (turn-car c)
  (accelerate-car c)
  (move-car c))


(define (turn-car c)
  (let ((current-theta (c 'get-theta)))
    (cond ((c 'turning-left?) ((c 'update-theta) (+ current-theta 15)))
          ((c 'turning-right?) ((c 'update-theta) (- current-theta 15)))
          (else ((c 'update-theta) current-theta)))))

(define (accelerate-car c)
  (let* ((theta (degrees->radians (c 'get-theta)))
        (current-vx (car (c 'get-velo)))
        (current-vy (cadr (c 'get-velo)))
        (new-vx (* .2  (cos theta)))
        (new-vy (* .2 (sin theta))))
    (cond ((and (c 'accel?) (c 'decel?)) ((c 'update-velo) (list current-vx current-vy)))
          ((c 'accel?) ((c 'update-velo) (list (+ current-vx new-vx) (- current-vy new-vy))))
          ((c 'decel?) ((c 'update-velo) (list (- current-vx new-vx) (+ current-vy new-vy))))
          (else ((c 'update-velo) (list current-vx current-vy))))))

(define (move-car c)
  (let ((x (c 'get-x))
        (y (c 'get-y))
        (vx (car (c 'get-velo)))
        (vy (cadr (c 'get-velo))))
    ((c 'update-pos) (list (+ x vx) (+ y vy)))))
          

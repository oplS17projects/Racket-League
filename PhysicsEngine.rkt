#lang racket
#|For testing|#
(define winSize (list 200 400))

(require "classes.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")
(provide left-turn)
(provide right-turn)
(provide accelerate-car)
(provide slow-car)
(provide update)

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
    (list (list nXpos nYpos) nvel (ftheta nvel))))

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

#|Getter for a particular axis' length|#
(define (getAxis axis)
  (if (equal? axis 'x) (car winSize)
      (cadr winSize)))

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
    (cond [(< max v) (list (- vx 0.001) (- vy 0.001))]
          [(= 0 v) (list 0 0)]
          [(> 0 v) (list 0 0)]
          [else (list (- (* max (cos c)) 0.001)
                      (- (* max (sin c)) 0.001))])))

#|Convert Keyboard input to car rotation / movement|#

#|Use 'get-velo to retreve velocity of car|#

#|
ftheta = calculates the angle

@param vel = velocity list
@return    = angle
|#

(define (ftheta vel)
  (let* ((vx (car vel))
         (vy (cadr vel))
         (v (findVelo (list vx vy))))
    (if (= v 0)
        0
        (radians->degrees (cos (/ vx v))))))

#|
thetaXY = converts an angle into x and y components

@param pts = an angle and velocity components
@return    = new velocity components
|#

(define (thetaXY pts)
  (let ((ang (car pts))
        (v (cadr pts)))
    (list (* v (cos ang))
          (* v (sin ang)))))

#|
findVelo = finds regular velocity

@param comp = components
@return     = value
|#

(define (findVelo comp)
  (sqrt (+ (* (car comp) (car comp))
           (* (cadr comp) (cadr comp)))))

#|
left-turn = turn the car left

@param num = which car
@return    = new velocity
|#

(define (left-turn num)
  (if (= num 1) (let ((ntheta (+ (car1 'get-theta) 1)))
                  ((car1 'update-car) (list (car1 'get-pos)
                                            (car1 'get-velo)
                                            ntheta)))
      (let ((ntheta (+ (car2 'get-theta) 1)))
        ((car2 'update-car) (list (car2 'get-pos)
                                  (car2 'get-velo)
                                  ntheta)))))

#|
right-turn = turn the car right

@param num = which car
@return    = new velocity
|#

(define (right-turn num)
  (if (= num 1) (let ((ntheta (- (car1 'get-theta) 1)))
                  ((car1 'update-car) (list (car1 'get-pos)
                                            (car1 'get-velo)
                                            ntheta)))
      (let ((ntheta (- (car2 'get-theta) 1)))
        ((car2 'update-car) (list (car2 'get-pos)
                                  (car2 'get-velo)
                                  ntheta)))))

#|
slow-car = slow the car

@param num = which car
@return    = new velocity
|#

(define (slow-car num)
  (list 0 0))

(define (slow-car-a num)
  (if (= num 1) (let ((vx (- (car ((car entities) 'get-velo)) 0.02))
                      (vy (- (cadr ((car entities) 'get-velo)) 0.02)))
                  (((car entities) 'update-car)(list ((car entities) 'get-pos)
                                                     (list vx vy)
                                                     ((car entities) 'theta))))
      (let ((vx (- (car ((cadr entities) 'get-velo)) 0.02))
            (vy (- (cadr ((cadr entities) 'get-velo)) 0.02)))
        (((cadr entities) 'update-car)(list ((cadr entities) 'get-pos)
                                            (list vx vy)
                                            ((cadr entities) 'theta))))))
       
#|
accelerate-car = accelerate the car

@param num = which car
@return    = new velocity
|#

(define (accelerate-car num)
  (car1 'get-theta))

(define (accelerate-car-s num)
  (if (= num 1) (let ((vx (+ (car ((car entities) 'get-velo)) 0.02))
                      (vy (+ (cadr ((car entities) 'get-velo)) 0.02)))
                  (((car entities) 'update-car)(list ((car entities) 'get-pos)
                                                     (list vx vy)
                                                     ((car entities) 'theta))))
      (let ((vx (+ (car ((cadr entities) 'get-velo)) 0.02))
            (vy (+ (cadr ((cadr entities) 'get-velo)) 0.02)))
        (((cadr entities) 'update-car)(list ((cadr entities) 'get-pos)
                                            (list vx vy)
                                            ((cadr entities) 'theta))))))

#|
update = updates positions and velocities

@param lst = list of everything
@return    = updated everything
|#

(define (update w)
  ((car1 'update-car) (carPos (car1 'get-pos) (car1 'get-velo)))
  #|((car2 'update-car) (carPos (car2 'get-pos) (car2 'get-velo)))
  ((ball 'update-car) (carPos (ball 'get-pos) (ball 'get-velo)))|#)

#|Ball physics.  Similar to car, but bounces off edges|#

#|
ballPos = Set the ball's new position

@param pos = the position of the ball
@param vel = the velocity of the ball
@param rad = the radius of the ball
@return    = List with car position and cdr velocity
|#
(define (ballPos pos vel rad)
  (let* ((nvel (currentvel (car vel) (cadr vel) 75))
         (nXpos (ballNewPos (car pos) (car nvel) rad 'x))
         (nYpos (ballNewPos (cadr pos) (cadr nvel) rad 'y)))
    (list (list nXpos nYpos) nvel)))

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
        [else (+ pos vel)]))

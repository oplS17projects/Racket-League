#lang racket

;; Racket Libraries

;; Custom Libraries

;; Provides
(provide getAxis)
(provide ftheta)
(provide findVelo)
(provide thetaXY)

#|Size of the world|#
(define winSize (list 1000 750))

#|Getter for a particular axis' length|#
(define (getAxis axis)
  (if (equal? axis 'x) (car winSize)
      (cadr winSize)))

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
        (checkTheta (radians->degrees (cos (/ vx v)))))))
#|
checkTheta = insures Theta is less than 360

@param theta = original angle
@return      = angle < 360
|#
(define (checkTheta theta)
  (if (> 360 theta) theta
      (checkTheta (- 360 theta))))

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
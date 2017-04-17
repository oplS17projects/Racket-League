#lang racket

(provide make-car)
(provide make-ball)

(require 2htdp/image)

(provide make-car)
(provide make-ball)
(provide make-boost)
(provide make-game)
(provide car-width)
(provide car-length)

(define car-width 100)
(define car-length 50)


(define (make-game)
  (let ((tics 0)
        (score '(0 0))
        (game-time 120)) ; Game total time in seconds
    (define (dispatch m)
      (cond ((eq? m 'get-tics) tics)
            ((eq? m 'tic) (set! tics (add1 tics)))
            ((eq? m 'get-score) score)
            ((eq? m 'left-score) (set! score (list (add1 (car score)) (cadr score))))
            ((eq? m 'right-score) (set! score (list (car score) (add1 (cadr score)))))
            ((eq? m 'get-timer) (text/font
                                (seconds->timer (- game-time (tics->seconds tics)))
                                60
                                "red"
                                #f
                                'modern
                                'normal
                                'bold
                                #f))
            ((eq? m 'get-scoreboard) (text/font
                                     (string-append (number->string (car score)) "      " (number->string (cadr score)))
                                     60
                                     "red"
                                     #f
                                     'modern
                                     'normal
                                     'bold
                                     #f))
            (else "Invalid message passed to game object.")))
    dispatch))

;Converts a number of seconds to a timer with minutes and seconds

(define (seconds->timer s)
  (let* ((mins (quotient s 60))
        (secs (remainder s 60))
        (min-string (number->string mins))
        (sec-string (number->string secs)))
    (string-append
     min-string
     ":"
     (if (= (string-length sec-string) 1)
         (string-append "0" sec-string)
         sec-string))))

; Converts an amount of clock tics to a number of seconds

(define (tics->seconds tic)
  (quotient tic 30))


(define (make-car pos theta name color)
  (let ((image (overlay/align "right" "middle" (rectangle (/ car-width 5) car-length "solid" "black")(rectangle car-width car-length "solid" color)))
        (velocity '(0 0))
        (boost 0)
        (accelerating #f)
        (decelerating #f))
    (define (update-c lst)
      (set! pos (car lst))
      (set! velocity (cadr lst))
      (set! theta (caddr lst)))
    (define (reset-c oPos oTheta)
      (update-c (list oPos '(0 0) oTheta)))      
    (define (dispatch m)
      (cond ((eq? m 'get-x) (car pos))
            ((eq? m 'get-y) (cadr pos))
            ((eq? m 'get-pos) pos)
            ((eq? m 'get-velo) velocity)
            ((eq? m 'update-car) update-c) ; Updates car's pos, velo, and theta. Takes list of lists : ((x y)(vx vy)theta)
            ((eq? m 'get-theta) theta)
            ((eq? m 'get-image) (rotate theta image))
            ((eq? m 'get-name) name)
            ((eq? m 'active?) #t) ; Used to see if object will get drawn to screen. For now, always true for cars and balls
            ((eq? m 'accel?) accelerating) ; get accel bool
            ((eq? m 'set-accel) (lambda (n) (set! accelerating n))) ; Turn accel bool on and off
            ((eq? m 'decel?) decelerating) ; get decel bool
            ((eq? m 'set-decel) (lambda (n) (set! decelerating n))) ; turn decel bool on and off
            ((eq? m 'reset) reset-c)
            (else "Invalid message passed to car object.")))
    dispatch))

(define (make-ball pos radius)
  (let ((image (circle radius "solid" "black"))
        (velocity '(5 5)))
    (define (update-b lst)
      (set! pos (car lst))
      (set! velocity (cadr lst)))
    (define (reset-b oPos)
      (update-b (list oPos '(0 0))))
    (define (dispatch m)
      (cond ((eq? m 'get-x) (car pos))
            ((eq? m 'get-y) (cadr pos))
            ((eq? m 'get-pos) pos)
            ((eq? m 'get-velo) velocity)
            ((eq? m 'get-radius) radius)
            ((eq? m 'update-ball) update-b) ; Updates ball's pos, velo. Takes list of lists : ((x y)(vx vy))
            ((eq? m 'get-image) image)
            ((eq? m 'active?) #t) ; Used to see if object will get drawn to screen. For now, always true for cars and balls
            ((eq? m 'reset) reset-b)
            (else "Invalid message passed to ball object.")))
    dispatch))
    
(define (make-boost pos)
  (let ((image (circle 20 "solid" "yellow"))
        (active #t)
        (recharge-time 0))
    (define (update-bst)
      (set! recharge-time 0)
      (set! active #t))
    (define (dispatch m)
      (cond ((eq? m 'get-x) (car pos))
            ((eq? m 'get-y) (cadr pos))
            ((eq? m 'get-pos) pos)
            ((eq? m 'get-radius) 20)
            ((eq? m 'active?) active) ; Used to know if boost is drawn to screen
            ((eq? m 'get-image) image)
            ((eq? m 'deactivate) (set! active #f) (set! recharge-time 120)) ; Used when a car collides with boost. Gets rid of boost and sets timer to full
            ((eq? m 'refresh) (if (= recharge-time 0)  ; Decrements the timer for new boost. Activates boost if 0
                                  (set! active #t)
                                  (set! recharge-time (- recharge-time 1))))
            ((eq? m 'reset) update-bst)
            (else "Invalid message passed to boost object.")))
    dispatch))

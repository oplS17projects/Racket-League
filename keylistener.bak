#lang racket

;; Exposed Procesdures
(provide key-handler)
(provide release-handler)

;; Racket Libraries
(require 2htdp/universe)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "VisualHandler.rkt")

(define (menu-key-listener w ke)
  (cond ((key=? ke " ") (space-key-pressed) w)
        (else w)))

(define (game-key-listener w ke)
  (cond ((key=? ke "left") (left-turn 2) w)
        ((key=? ke "right") (right-turn 2) w)
        ((key=? ke "down") (begin ((car2 'set-decel) #t) (slow-car 2)) w)
        ((key=? ke "up") (begin ((car2 'set-accel) #t) (accelerate-car 2)) w)
        ((key=? ke "a") (left-turn 1) w)
        ((key=? ke "d") (right-turn 1) w)
        ((key=? ke "s") (begin ((car2 'set-decel) #t) (slow-car 2)) w)
        ((key=? ke "w") (begin ((car1 'set-accel) #t) (accelerate-car 1)) w)
        ((key=? ke "escape") (escape-key-pressed) w)
        (else w)))

(define (menu-release-listener w key)
  w)

(define (game-release-listener w key)
  (cond ((key=? key "up") (begin
                            ((car2 'set-accel) #f)
                            w))
        ((key=? key "down") (begin
                              ((car2 'set-decel) #f)
                              w))
        ((key=? key "w") (begin
                           ((car1 'set-accel) #f)
                           w))
        ((key=? key "a") (begin
                           ((car1 'set-decel) #f)))
        (else w)))
  
;; Handles when a key event is found
(define (key-handler w ke)
    (if (menu-state 'ShowMenu?)
        (menu-key-listener w ke)
        (game-key-listener w ke)))

;; Handles when a key release happens
(define (release-handler w key)
  (if (menu-state 'ShowMenu?)
      (menu-release-listener w key)
      (game-release-listener w key)))

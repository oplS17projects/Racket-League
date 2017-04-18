#lang racket

;; Exposed Procesdures
(provide key-handler)

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
  
;; Handles when a key event is found
(define (key-handler w ke)
    (if (menu-state 'ShowMenu?)
        (menu-key-listener w ke)
        (game-key-listener w ke)))

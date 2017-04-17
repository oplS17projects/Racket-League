#lang racket

;; Exposed Procesdures
(provide key-handler)

;; Racket Libraries
(require 2htdp/universe)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "VisualHandler.rkt")

(define (menu-key-listener w ke)
  (cond ;;((key=? ke "up") (menu-up-pressed))
        ;;((key=? ke "down") (menu-down-pressed))
        ((key=? ke " ") (enter-key-pressed))
        (else w)))

(define (game-key-listener w ke)
  (cond ((key=? ke "left") (left-turn 2) w)
        ((key=? ke "right") (right-turn 2) w)
        ((key=? ke "down") (slow-car 2) w)
        ((key=? ke "up") (accelerate-car 2) w)
        ((key=? ke "a") (left-turn 1) w)
        ((key=? ke "d") (right-turn 1) w)
        ((key=? ke "s") (slow-car 1) w)
        ((key=? ke "w") (accelerate-car 1) w)
        (else w)))
  
;; Handles when a key event is found
(define (key-handler w ke)
    (if (menu-state 'ShowMenu?)
        (menu-key-listener w ke)
        (game-key-listener w ke)))

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
        ((key=? ke " ") (enter-key-pressed) w)
        (else w)))

(define (game-key-listener w ke)
  (cond ((key=? ke "left") (left-turn 2))
        ((key=? ke "right") (right-turn 2))
        ((key=? ke "down") (slow-car 2))
        ((key=? ke "up") (accelerate-car 2))
        ((key=? ke "a") (left-turn 1))
        ((key=? ke "d") (right-turn 1))
        ((key=? ke "s") (slow-car 1))
        ((key=? ke "w") (accelerate-car 1))
        (else "Nothing to do")))
  
;; Handles when a key event is found
(define (key-handler w ke)
    (if (menu-state 'ShowMenu?)
        (menu-key-listener w ke)
        (game-key-listener w ke)))

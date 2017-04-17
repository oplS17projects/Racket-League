#lang racket

;; Exposed Procesdures
(provide key-handler)

;; Racket Libraries
(require 2htdp/universe)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")

(define (menu-key-listener w ke)
  (cond ((key=? ke " ") (space-key-pressed) w)
        (else w)))

(define (game-key-listener w ke)
  (cond ((key=? ke "left") (left-turn 2) ((sound-engine 'play-sound-effect) 'goal-scored))
        ((key=? ke "right") (right-turn 2))
        ((key=? ke "down") ((car2 'set-decel) #t))
        ((key=? ke "up") ((car2 'set-accel) #t))
        ((key=? ke "a") (left-turn 1))
        ((key=? ke "d") (right-turn 1))
        ((key=? ke "s") ((car1 'set-decel) #t))
        ((key=? ke "w") ((car1 'set-accel) #t))
        ((key=? ke "escape") (escape-key-pressed))
        (else "Nothing to do")))
  
;; Handles when a key event is found
(define (key-handler w ke)
    (if (menu-state 'ShowMenu?)
        (menu-key-listener w ke)
        (game-key-listener w ke)))

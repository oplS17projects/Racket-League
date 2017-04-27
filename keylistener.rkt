#lang racket

;; Exposed Procesdures
(provide key-handler)
(provide release-handler)

;; Racket Libraries
(require 2htdp/universe)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")

(define (menu-key-listener w ke)
  (cond ((key=? ke " ") (space-key-pressed) w)
        ((key=? ke "up") (up-key-pressed) w)
        ((key=? ke "down") (down-key-pressed) w)
        (else w)))

(define (game-key-listener w ke)
  (let ((sound-engine (create-sound-engine)))
    (cond ((key=? ke "left") (begin
                               ((car2 'turn-true) 'left)
                               w))
          ((key=? ke "right") (begin
                                ((car2 'turn-true) 'right)
                                w))
          ((key=? ke "down") (if (car2 'decel?)
                                 w
                                 (begin
                                   ((car2 'set-decel) #t)
                                   w)))
          ((key=? ke "up") (if (car2 'accel?)
                               w
                               (begin
                                 ((car2 'set-accel) #t)
                                 ((sound-engine 'play-sound-effect) 'accelerate)
                                 w)))
          ((key=? ke "a") (begin
                           ((car1 'turn-true) 'left)
                           w))
          ((key=? ke "d") (begin
                            ((car1 'turn-true) 'right)
                            w))
          ((key=? ke "s") (if (car1 'decel?)
                              w
                              (begin
                                ((car1 'set-decel) #t)
                                w)))
          ((key=? ke "w") (if (car1 'accel?)
                              w
                              (begin
                                ((car1 'set-accel) #t)
                                ((sound-engine 'play-sound-effect) 'accelerate)
                                w)))
          ((key=? ke "escape") (begin
                                 (escape-key-pressed)
                                 w))
          (else w))))

(define (menu-release-listener w key)
  w)

(define (game-release-listener w key)
  (cond ((key=? key "up") (begin
                            ((car2 'set-accel) #f)
                            w))
        ((key=? key "left") (begin
                             (car2 'stop-turning)
                             w))
        ((key=? key "right") (begin
                                (car2 'stop-turning)
                                w))
        ((key=? key "down") (begin
                             ((car2 'set-accel) #f)
                             w))
        ((key=? key "w") (begin
                           ((car1 'set-accel) #f)
                           w))
        ((key=? key "a") (begin
                          (car1 'stop-turning)
                          w))
        ((key=? key "d") (begin
                          (car1 'stop-turning)
                          w))
        ((key=? key "a") (begin
                           ((car1 'set-decel) #f)))
        (else w)))

(define (game-over-handler w key)
  (if (key=? key " ")
      (begin
        (game-over-state 'ResetGameOver)
        ((sound-engine 'play-music-effect) 'menu-music)
        (menu-state 'SwitchToMenu))
      "Nothing to do"))
      
  
;; Handles when a key event is found
(define (key-handler w ke)
  (cond ((menu-state 'ShowMenu?)
           (menu-key-listener w ke))
          ((game-over-state 'IsGameOver?) (game-over-handler w ke))
          (else (game-key-listener w ke))))

;; Handles when a key release happens
(define (release-handler w key)
  (if (menu-state 'ShowMenu?)
      (menu-release-listener w key)
      (game-release-listener w key)))

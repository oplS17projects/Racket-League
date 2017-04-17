#lang racket

;; Exposed Procedures

;; Racket Libraries
(require 2htdp/image)
(require 2htdp/universe)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "keylistener.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")

((sound-engine 'play-music-effect) 'menu-music)

(big-bang 0
          (on-key key-handler)
          (on-tick update .03)
          (to-draw draw-entities)
          (name "Racket-League"))


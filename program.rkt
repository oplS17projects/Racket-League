#lang racket

;; Exposed Procedures

;; Racket Libraries
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "BallEngine.rkt")
(require "keylistener.rkt")
(require "VisualHandler.rkt")
(require "soundengine.rkt")

(big-bang 0
          (on-key key-handler)
          (on-release release-handler)
          (on-tick update .03)
          (to-draw draw-entities)
          (name "Racket-League"))

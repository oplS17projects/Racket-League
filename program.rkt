#lang racket

;; Exposed Procedures

;; Racket Libraries
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)

;; Custom Libraries
(require "Source/PhysicsEngine/PhysicsEngine.rkt")
(require "Source/PhysicsEngine/BallEngine.rkt")
(require "Source/KeyListener/keylistener.rkt")
(require "Source/VisualEngine/VisualHandler.rkt")
(require "Source/SoundEngine/soundengine.rkt")

((sound-engine 'play-music-effect) 'menu-music)

(big-bang 0
          (on-key key-handler)
          (on-release release-handler)
          (on-tick update .03)
          (to-draw draw-entities)
          (name "Racket-League"))


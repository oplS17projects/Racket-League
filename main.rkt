#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(require "classes.rkt")
(require "soundengine.rkt")
(require "PhysicsEngine.rkt")
(require "VisualHandler.rkt")
(require "keylistener.rkt")

(define sound-engine (create-sound-engine))

(big-bang 0
          (on-key key-handler)
          (to-draw draw-entities))


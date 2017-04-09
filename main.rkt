#lang racket

;; Exposed Procedures

;; Racket Libraries
(require 2htdp/image)
(require 2htdp/universe)

;; Custom Libraries
(require "PhysicsEngine.rkt")
(require "keylistener.rkt")

(big-bang 0
          (display-mode 'fullscreen)
          (on-key key-handler)
          (on-tick update-world .03)
          (to-draw draw-entities))


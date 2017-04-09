#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(provide key-handler)
(require "soundengine.rkt")

(define (play-world-sound world sound-name)
  (let ((sound-engine (create-sound-engine)))
    (cond ((eq? sound-name 'stop) (sound-engine 'stop) world)
          (else ((sound-engine 'play-sound-effect) sound-name) world))))

(define (key-handler w ke)
  (cond ((or (key=? ke "left") (key=? ke "right")) (play-world-sound w 'goal-scored))
        ((key=? ke "escape") (play-world-sound w 'stop))
        (else w)))

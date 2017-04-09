#lang racket

(require rsound)
(require 2htdp/universe)
(require 2htdp/image)

(require "soundengine.rkt")

(define (create-sample-scene height)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 height UFO))

(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))

(define (play-world-sound world sound-name)
  (let ((sound-engine (create-sound-engine)))
    (cond ((eq? sound-name 'stop) (sound-engine 'stop) world)
          (else ((sound-engine 'play-sound-effect) sound-name) world))))

(define (play-world-music world sound-name)
  (let ((sound-engine (create-sound-engine)))
    (cond ((eq? sound-name 'stop) (sound-engine 'stop) world)
          (else ((sound-engine 'play-music-effect) sound-name) world))))

(define (key-handler w ke)
  (cond ((key=? ke "left") (play-world-sound w 'goal-scored))
        ((key=? ke "right") (play-world-music w 'menu-music))
        ((key=? ke "escape") (play-world-sound w 'stop) (play-world-music w 'stop))
        (else w)))

(big-bang 0
          (on-key key-handler)
          (to-draw create-sample-scene))

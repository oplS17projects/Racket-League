#lang racket

(require (file "classes.rkt"))

(require 2htdp/universe)
(require 2htdp/image)
(require "soundengine.rkt")

(provide car1)
(provide car2)
(provide ball)
(provide entities)
(provide background)
(provide draw-entities)

(define car1 (make-car '(200 375) 0 "Player1" "blue"))

(define car2 (make-car '(800 375) 180 "Player2" "orange"))

(define ball (make-ball '(500 375) 15))

(define entities (list car1 car2 ball))

(define background (bitmap/file "Field.png"))

(define (draw-entities t)
    (define (rhelp lst scene)
      (if (null? lst)
          scene
          (rhelp (cdr lst) (place-image  ((car lst) 'get-image) ((car lst) 'get-x)  ((car lst) 'get-y) scene))))
    (rhelp entities background))

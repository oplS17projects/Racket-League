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
(provide menu-state)
(provide space-key-pressed)
(provide escape-key-pressed)

(define car1 (make-car '(200 375) 0 "Player1" "blue"))

(define car2 (make-car '(800 375) 180 "Player2" "orange"))

(define ball (make-ball '(500 375) 15))

(define entities (list car1 car2 ball))

(define background (bitmap/file "Field.png"))

(define menu-background (bitmap/file "Menu.png"))

(define (create-menu-state)
  (let ((shouldShow #t))
    (define (ShowMenu?)
      shouldShow)
    (define (SwitchToGame)
      (begin (set! shouldShow #f)
             (sound-engine 'stop)
             ((sound-engine 'play-sound-effect) 'start-game)))
    (define (SwitchToMenu)
      (begin (set! shouldShow #t)
             (sound-engine 'stop)
             ((sound-engine 'play-sound-effect) 'stop-game)
             ((sound-engine 'play-music-effect) 'menu-music)))
    (define (dispatch message)
      (cond ((eq? message 'ShowMenu?) (ShowMenu?))
            ((eq? message 'SwitchToGame) (SwitchToGame))
            ((eq? message 'SwitchToMenu) (SwitchToMenu))
            (else (error "Could Not Determine Menu State"))))
    dispatch))

(define (space-key-pressed)
  (if (= 0 (menu 'get-selection))
      (menu-state 'SwitchToGame)
      "Nothing to do"))

(define (escape-key-pressed)
  ((sound-engine 'play-music-effect) 'menu-music)
  (menu-state 'SwitchToMenu))

(define menu-state (create-menu-state))

(define (create-menu)
  (let ((selection 0))
    (define (get-selection)
      selection)
    (define (update-selection number)
      (begin (set! selection (+ selection number))))
    (define (dispatch message)
      (cond ((eq? message 'get-selection) (get-selection))
            ((eq? message 'update-selection) update-selection)
            (else (error "Could Not Communicate With Menu"))))
    dispatch))

(define menu (create-menu))

(define (draw-menu scene)
  scene)

(define (draw-entities t)
    (if (menu-state 'ShowMenu?)
        menu-background
        (rhelp entities background)))

(define (rhelp lst scene)
      (if (null? lst)
          scene
          (rhelp (cdr lst)
                 (place-image  ((car lst) 'get-image)
                               ((car lst) 'get-x)
                               ((car lst) 'get-y)
                               scene))))

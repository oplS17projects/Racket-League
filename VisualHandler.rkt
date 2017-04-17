#lang racket

(require (file "classes.rkt"))

(require 2htdp/universe)
(require 2htdp/image)

(require "soundengine.rkt")

;; Provides
(provide car1)
(provide car2)
(provide ball)
(provide entities)
(provide boost-list)
(provide background)
(provide menu-background)
(provide draw-entities)
(provide menu-state)
(provide space-key-pressed)
(provide escape-key-pressed)

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
             (if ((car lst) 'active?)
                 (place-image  ((car lst) 'get-image)
                               ((car lst) 'get-x)
                               ((car lst) 'get-y)
                               scene)
                 scene))))

;; Global Objects

;; The blue car object
(define car1 (make-car '(200 375) 0 "Player1" "blue"))

;; The orange car object
(define car2 (make-car '(800 375) 180 "Player2" "orange"))

;; The ball object
(define ball (make-ball '(500 375) 15))

;; The boost objects
(define boost1 (make-boost '(30 30)))
(define boost2 (make-boost '(30 720)))
(define boost3 (make-boost '(970 30)))
(define boost4 (make-boost '(970 720)))

;; The list of entities
(define entities (list car1 car2 ball boost1 boost2 boost3 boost4))

;; The list of boost objects
(define boost-list (list boost1 boost2 boost3 boost4))

;; The background image
(define background (bitmap/file "Field.png"))

;; The menu image
(define menu-background (bitmap/file "Menu.png"))

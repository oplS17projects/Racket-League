#lang racket

(require (file "classes.rkt"))

(require 2htdp/universe)
(require 2htdp/image)

(require "soundengine.rkt")
(require "Menu.rkt")

;; Provides
(provide car1)
(provide car2)
(provide ball)
(provide entities)
(provide boost-list)
(provide background)
(provide draw-entities)
(provide world-state)
(provide menu-background)
(provide menu-state)
(provide menu)
(provide space-key-pressed)
(provide escape-key-pressed)
(provide up-key-pressed)
(provide down-key-pressed)
(provide game-over-state)

(define (draw-menu scene)
  (let ((selection-box (menu 'get-selection-box)))
    (cond ((= 1 (menu 'get-selection))
             (place-image selection-box
                          490
                          285
                          scene))
            ((= 0 (menu 'get-selection))
             (place-image selection-box
                          495
                          453
                          scene))
            (else scene))))

(define (draw-entities t)
    (if (menu-state 'ShowMenu?)
        (if (menu-state 'ShouldExit?)
            exit-background
            (draw-menu menu-background))
        (if (game-over-state 'IsGameOver?)
            game-over-background
            (rhelp entities (place-image (world-state 'get-scoreboard)
                                         500
                                         30                                     
                                         (place-image (world-state 'get-timer)
                                                      500
                                                      720
                                                      background))))))
  
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

(define (create-menu-state)
  (let ((shouldShow #t)
        (shouldExit #f))
    (define (SwitchToGame)
      (begin (set! shouldShow #f)
             (reset)
             (world-state 'reset-game)))
             ;(sound-engine 'stop)
             ;((sound-engine 'play-sound-effect) 'start-game)))
    (define (SwitchToMenu)
      (begin (set! shouldShow #t)
             (sound-engine 'stop)))
             ;((sound-engine 'play-sound-effect) 'stop-game)
             ;((sound-engine 'play-music-effect) 'menu-music)))
    (define (ExitGame)
      (begin
        (set! shouldExit #t)))
        ;(sound-engine 'stop)))
    (define (dispatch message)
      (cond ((eq? message 'ShowMenu?) shouldShow)
            ((eq? message 'SwitchToGame) (SwitchToGame))
            ((eq? message 'SwitchToMenu) (SwitchToMenu))
            ((eq? message 'ExitGame) (ExitGame))
            ((eq? message 'ShouldExit?) shouldExit)
            (else (error "Could Not Determine Menu State"))))
      dispatch))

(define (create-game-over)
  (let ((shouldShow #f))
    (define (check-for-game-over)
      (if (and (not (menu-state 'ShowMenu?))
               (>= 0 (- (world-state 'get-game-time) (tics->seconds (world-state 'get-tics)))))
          (set! shouldShow #t)
          "Nothing to do"))
    (define (dispatch message)
      (cond ((eq? message 'IsGameOver?) shouldShow)
            ((eq? message 'check-for-game-over) (if (not (menu-state 'ShowMenu?))
                                                    (check-for-game-over)
                                                    "Nothing to do"))
            ((eq? message 'ResetGameOver) (set! shouldShow #f))
            (else "Nothing to do")))
    dispatch))

(define (create-menu)
  (let ((selection 1)
        (selection-box (rectangle 385 105 "outline" "white")))
    (define (update-selection type)
      (begin
        (cond ((eq? 'up type) (set! selection (+ selection 1)))
              ((eq? 'down type) (set! selection (- selection 1)))
              (else "Nothing to change"))
        (cond ((> 1 selection) (set! selection 0))
              ((< 0 selection) (set! selection 1)))))
    (define (dispatch message)
      (cond ((eq? message 'get-selection) selection)
            ((eq? message 'update-selection-up) (update-selection 'up))
            ((eq? message 'update-selection-down) (update-selection 'down))
            ((eq? message 'get-selection-box) selection-box)
            (else (error "Could Not Communicate With Menu"))))
    dispatch))

(define (space-key-pressed)
  (cond ((= 1 (menu 'get-selection)) (menu-state 'SwitchToGame))
        ((= 0 (menu 'get-selection)) (menu-state 'ExitGame))
        (else "Nothing to do")))

(define (escape-key-pressed)
  ;((sound-engine 'play-music-effect) 'menu-music)
  (menu-state 'SwitchToMenu))

(define (up-key-pressed)
  (menu 'update-selection-up))

(define (down-key-pressed)
  (menu 'update-selection-down))

(define (reset)
  (begin
    ((car1 'reset) '(200 375) 0)
    ((car2 'reset) '(800 375) 180)
    ((ball 'reset) '(500 375))
    (boost1 'rest)
    (boost2 'reset)
    (boost3 'reset)
    (boost4 'reset)))

;; The state of the menu
(define menu-state (create-menu-state))

;; The menu
(define menu (create-menu))

;; The menu image
(define menu-background (bitmap/file "Menu.png"))

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

;;State of game
(define world-state (make-game))

(define game-over-state (create-game-over))

(define exit-background (bitmap/file "Exit.png"))

(define game-over-background (bitmap/file "GameOver.png"))

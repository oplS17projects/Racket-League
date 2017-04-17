#lang racket

(require (file "classes.rkt"))

(require 2htdp/universe)
(require 2htdp/image)
(require "soundengine.rkt")

(provide car1)
(provide car2)
(provide ball)
(provide entities)
(provide boost-list)
(provide background)
(provide draw-entities)
(provide menu-state)
(provide enter-key-pressed)

(define car1 (make-car '(200 375) 0 "Player1" "blue"))

(define car2 (make-car '(800 375) 180 "Player2" "orange"))

(define boost1 (make-boost '(30 30)))

(define boost2 (make-boost '(30 720)))

(define boost3 (make-boost '(970 30)))

(define boost4 (make-boost '(970 720)))

(define ball (make-ball '(500 375) 15))

(define entities (list car1 car2 ball boost1 boost2 boost3 boost4))

(define boost-list (list boost1 boost2 boost3 boost4))

(define background (bitmap/file "Field.png"))

(define (create-menu-state)
  (let ((shouldShow #f))
    (define (ShowMenu?)
      shouldShow)
    (define (SwitchToGame)
      (begin (set! shouldShow #f)))
    (define (SwitchToMenu)
      (begin (set! shouldShow #t)))
    (define (dispatch message)
      (cond ((eq? message 'ShowMenu?) (ShowMenu?))
            ((eq? message 'SwitchToGame) (SwitchToGame))
            ((eq? message 'SwitchToMenu) (SwitchToMenu))
            (else (error "Could Not Determine Menu State"))))
    dispatch))

(define (enter-key-pressed)
  (if (= 0 (menu 'get-selection))
      (menu-state 'SwitchToGame)
      "Nothing to do"))

(define menu-state (create-menu-state))

(define (create-menu)
  (let ((selection 0)
        (start-box 0))
    (define (get-selection)
      selection)
    (define (update-selection number)
      (begin (set! selection (+ selection number))))
    (define (get-start-box)
      start-box)
    (define (dispatch message)
      (cond ((eq? message 'get-selection) (get-selection))
            ((eq? message 'update-selection) update-selection)
            ((eq? message 'get-start-box) (get-start-box))
            (else (error "Could Not Communicate With Menu"))))
    dispatch))

(define menu (create-menu))

(define (draw-menu scene)
  scene)

(define (draw-entities t)
  (let ((menu-state (create-menu-state)))
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
    (if (menu-state 'ShowMenu?)
        (draw-menu background)
        (rhelp entities background))))

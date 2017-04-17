#lang racket

;; Exposed Procedures
(provide sound-engine)

;; Racket libraries
(require rsound)
(require racket/require)

;; Custom Libraries
(require "soundsources.rkt")

;; Creates a new sound engine
(define (create-sound-engine)
  (let ((sound-stream (make-pstream))
        (music-stream (make-pstream)))
    (define (play-sound-effect sound-name)
      (pstream-play sound-stream
                    (find-effect sound-name (sound-effects))))
    (define (play-music-effect music-name)
      (pstream-play music-stream
                    (find-effect music-name (music-effects))))
    (define (engine procedure-name)
      (cond ((eq? procedure-name 'play-sound-effect) play-sound-effect)
            ((eq? procedure-name 'play-music-effect) play-music-effect)
            ((eq? procedure-name 'stop) (stop))
            (else (error "Could Not Communicate With Sound Engine"))))
    engine))

;; Finds the effects name in a given list based upon a symbol
(define (find-effect effects-name effects-list)
  (effects-list-iter effects-name (car effects-list) (cdr effects-list)))

;; Iterates through a list to find effects name
(define (effects-list-iter effects-name current-item rest-of-list)
  (cond ((eq? (car current-item) effects-name) (cadr current-item))
        ((null? rest-of-list) (error "Could Not Find Effect"))
        (else (effects-list-iter effects-name (car rest-of-list) (cdr rest-of-list)))))

;; The instantiated sound-engine we want to use throughout to handle music
(define sound-engine (create-sound-engine))

;; This code handles playing music or sound effects using the RSound library
;; The only code that we want to expose for use is the create-sound engine.
;; This will create a sound engine with the proper procedures exposed for use within
;; procedures external to this code.

;; How it works
;;
;; Overview
;; Calling create-sound-engine will return a procedure that will be used to call other internal procedures
;; For the most part, you must specify whether you want to play music or play a sound using a symbol
;; 'play-sound-effect or 'play-music-effect
;; These will then tell the sound-engine to return another procedure. This procedure takes in the effect
;; name you want to play through a symbol
;; 'goal-scored
;; The sound you want to play is then searched for in a list with symbol rsound pairs. If checks to see
;; if the symbol matches what you wanted to play (looks for 'goal-scored within the list) and if it finds a match
;; if will return the rsound object that goes with that symbol.
;; Finally, the sound-engine calls pstream-play to play the sound effect

;; How it looks
;; (define sound-engine (create-sound-engine))
;; ((sound-engine 'play-sound-effect) 'goal-scored)

;; This will properly play the sound as long as the sound name is within the list of pairs

;; Need to redesign for better testability. Currently procedures are nested and cannot be properly tested
;; due to the fact that they call other procedures.

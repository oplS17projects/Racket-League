#lang racket

(require rsound)
(require racket/require)

(provide create-sound-engine)
(require "soundsources.rkt")

(define (create-sound-engine)
  (let ((sound-stream (make-pstream))
        (music-stream (make-pstream)))
    (define (play-sound-effect sound-name)
      (pstream-play sound-stream
                    (find-sound-effect sound-name)))
    (define (play-music-effect music-name)
      (pstream-play music-stream
                    (find-music-effect music-name)))
    (define (engine procedure-name)
      (cond ((eq? procedure-name 'play-sound-effect) play-sound-effect)
            ((eq? procedure-name 'play-music-effect) play-music-effect)
            ((eq? procedure-name 'stop) (stop))
            (else (error "Could Not Communicate With Sound Engine"))))
    engine))

(define (find-sound-effect sound-name)
  (let ((usable-sounds (sound-effects)))
    (define (sound-iteration current-item rest-of-list)
      (cond ((eq? (car current-item) sound-name) (cadr current-item))
            ((null? rest-of-list) (error "Could Not Find Sound Effect"))
            (else (sound-iteration (car rest-of-list) (cdr rest-of-list)))))
    (sound-iteration (car usable-sounds) (cdr usable-sounds))))

(define (find-music-effect music-name)
    (let ((usable-music (music-effects)))
    (define (music-iteration current-item rest-of-list)
      (cond ((eq? (car current-item) music-name) (cadr current-item))
            ((null? rest-of-list) (error "Could Not Find Music Effect"))
            (else (music-iteration (car rest-of-list) (cdr rest-of-list)))))
    (music-iteration (car usable-music) (cdr usable-music))))

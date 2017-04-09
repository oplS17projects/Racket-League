#lang racket

(require rsound)

(provide sound-effects)
(provide music-effects)

(define (sound-effects)
  (list
   (list 'goal-scored (rs-read "goal.wav"))
   (list 'turn-car (rs-read "turn.wav"))))

(define (music-effects)
  (list
   (list 'menu-music (rs-read "menu_music.wav"))))



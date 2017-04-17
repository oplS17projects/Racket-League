#lang racket

(require rsound)

(provide sound-effects)
(provide music-effects)

(define (sound-effects)
  (list
   (list 'goal-scored (rs-read "goal.wav"))
   (list 'explosion (rs-read "explosion.wav"))
   (list 'accelerate (rs-read "acceleration.wav"))
   (list 'blue-scored (rs-read "blue-player-scored.wav"))
   (list 'orange-scored (rs-read "orange-player-scored.wav"))
   (list 'start-game (rs-read "start-game.wav"))
   (list 'stop-game (rs-read "stop-game.wav"))
   (list 'small-collision (rs-read "small-collision.wav"))))

(define (music-effects)
  (list
   (list 'menu-music (rs-read "menu_music.wav"))))



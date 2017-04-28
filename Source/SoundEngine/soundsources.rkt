#lang racket

(require rsound)

(provide sound-effects)
(provide music-effects)

(define (sound-effects)
  (list
   (list 'goal-scored (rs-read "../../Resources/Sound/goal.wav"))
   (list 'explosion (rs-read "../../Resources/Sound/explosion.wav"))
   (list 'accelerate (rs-read "../../Resources/Sound/acceleration.wav"))
   (list 'blue-scored (rs-read "../../Resources/Sound/blue-player-scored.wav"))
   (list 'orange-scored (rs-read "../../Resources/Sound/orange-player-scored.wav"))
   (list 'start-game (rs-read "../../Resources/Sound/start-game.wav"))
   (list 'stop-game (rs-read "../../Resources/Sound/stop-game.wav"))
   (list 'small-collision (rs-read "../../Resources/Sound/small-collision.wav"))))

(define (music-effects)
  (list
   (list 'menu-music (rs-read "../../Resources/Sound/menu_music.wav"))))



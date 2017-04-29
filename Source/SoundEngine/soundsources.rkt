#lang racket

(require rsound)

(provide sound-effects)
(provide music-effects)

(define (sound-effects)
  (list
   (list 'goal-scored (rs-read (build-path (current-directory) "Resources" "Sound" "goal.wav")))
   (list 'explosion (rs-read (build-path (current-directory) "Resources" "Sound" "explosion.wav")))
   (list 'accelerate (rs-read (build-path (current-directory) "Resources" "Sound" "acceleration.wav")))
   (list 'blue-scored (rs-read (build-path (current-directory) "Resources" "Sound" "blue-player-scored.wav")))
   (list 'orange-scored (rs-read (build-path (current-directory) "Resources" "Sound" "orange-player-scored.wav")))
   (list 'start-game (rs-read (build-path (current-directory) "Resources" "Sound" "start-game.wav")))
   (list 'stop-game (rs-read (build-path (current-directory) "Resources" "Sound" "stop-game.wav")))
   (list 'small-collision (rs-read (build-path (current-directory) "Resources" "Sound" "small-collision.wav")))))

(define (music-effects)
  (list
   (list 'menu-music (rs-read (build-path (current-directory) "Resources" "Sound" "menu_music.wav")))))



(ns d-splatkin.songs.fus
  (:use [overtone.live]
        [overtone.inst.drum]
        [d-splatkin.instruments]))

;; Mix

(def bleep-dist (inst-fx! bleep fx-distortion2))
(def bleep-verb (inst-fx! bleep fx-freeverb))
(ctl bleep-verb :wet-dry 0.75 :room-size 0.8 :dampening 1)

(def drops-echo (inst-fx! drops fx-echo))

(volume 2)

;; Sequencer
(def metro (metronome 128))

(defn make-bar []
  (def bar {0 [drops kick]
            (ranged-rand 0.25 0.5) [drops]
            (ranged-rand 0.25 1) [drops]
            3.5 [kick]
            4 [kick]

            5 [drops]
            (ranged-rand 5.5 6) [drops]
            6 [kick]}))

;; need bar map to change randomly every bar

; For every tick of the metronome, we loop through all our beats
; and find the apropriate one my taking the metronome tick mod 4.
; Then we play all the instruments for that beat.

(defn rhythm-player
  [tick bar]
  (dorun
   (for [k (keys bar)]
      (let [beat (Math/floor k)
            offset (- k beat)]
           (if (== 0 (mod (- tick beat) 8))
               (let [instruments (bar k)]
                    (dorun
                      (for [instrument instruments]
                        (at (metro (+ offset tick)) (instrument))))))))))

(defn player [beat]
  (let [next-beat (+ beat 8)]
    (at (metro beat) (pretty-bell 70 2 2))
    (at (metro (+ beat 1)) (pretty-bell 73 2 2))
    (at (metro (+ beat 2)) (pretty-bell 71 2 2))
    (at (metro (+ beat 3)) (pretty-bell 77 2 2))
    (at (metro (+ beat 4)) (pretty-bell 75 2 2))
    (apply-at (metro next-beat) #'player [next-beat])))


;; define a run fn which will call our player fn for each beat and will schedule
;; itself to be called just before the next beat

(defn run-rhythm
  [m]
  (make-bar)
  (let [beat (m)]
    (rhythm-player beat bar)
    (apply-at (m (inc beat))  #'run-rhythm [m])))


(run-rhythm metro)
(player (metro))
(stop)




;; Keyboard Stuff
(def kb (midi-in ))

(defn midi-player [event ts]
  (alien-pad (:note event) (/ (:vel event) 128.0)))

(midi-handle-events kb #'midi-player)

(event-debug-on)

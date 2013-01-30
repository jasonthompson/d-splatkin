(ns d-splatkin.songs.fus
  (:use [overtone.live]
        [overtone.inst.drum]
        [d-splatkin.instruments]))


(definst s101 [note 60 amp 0.33 bpm 120 note-multiplier 1.1 trig-rate 3 pulse-width 0.8]
  (let [notes   [note (* note note-multiplier)]
        trig    (impulse:kr trig-rate)
        freq    (midicps (lag (demand trig 0 (dxrand notes INF)) 0.25))
        swr     (demand trig 0 (dseq [1 2 1] INF))
        sweep   (lin-exp (lf-tri swr) -1 1 40 3000)
        src     (pulse [freq (* freq pulse-width)] pulse-width)
        src     (lpf src sweep)]
    (* amp src)))

(s101 40 0.2)
(stop)
;; Mix

(def bleep-dist (inst-fx! bleep fx-distortion2))
(def bleep-verb (inst-fx! bleep fx-freeverb))
(ctl bleep-verb :wet-dry 0.75 :room-size 0.8 :dampening 1)

(def drops-echo (inst-fx! drops fx-echo))
(ctl drops-echo :decay-time 2 :delay-time 0.125)

(def s101-dist (inst-fx! s101 fx-distortion2))
(ctl s101-dist :amount 0.75)
(def s101-chorus (inst-fx! s101 fx-chorus))
(ctl s101-chorus :rate 0.75 :depth 0.75)
(volume 2)

;; Sequencer
(def metro (metronome 128))

(defn make-bar []
  (def bar {0 [drops]
            0.125 [drops]
            (ranged-rand 0.25 0.5) [drops]
            (ranged-rand 0.25 1) [drops]
            4 [drops]

            5 [drops]
            (ranged-rand 5.5 6) [drops]
            }))

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
    (at (metro (+ beat 0.25)) (pretty-bell 73 2 2))
    (at (metro (+ beat 0.75)) (pretty-bell 71 2 2))
    (at (metro (+ beat 1)) (pretty-bell 78 2 2))

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

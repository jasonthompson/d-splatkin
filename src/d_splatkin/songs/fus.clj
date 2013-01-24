(ns d-splatkin.songs.fus
  (:use [overtone.live]
        [overtone.inst.drum]))


(definst bleep [note 60]
  (let [freq (midicps note)
        f-env (line:kr 1 0 0.1 :action FREE)]
    (square (* freq f-env))))


(def bleep-dist (inst-fx! bleep fx-distortion2))
(def bleep-verb (inst-fx! bleep fx-freeverb))
(ctl bleep-verb :wet-dry 0.75 :room-size 0.8 :dampening 1)


(def dry-kick-echo (inst-fx! dry-kick fx-echo))

(inst-volume! bleep 0.5)
(inst-volume! dry-kick 1)


(clear-fx bleep)
(stop)

(volume 8)

;; Sequencer
(def metro (metronome 128))

; Our bar is a map of beat to instruments to play

(defn make-bar []
  (def bar {0 [bleep]
            0.125 [bleep]
            1 [bleep]
            (ranged-rand 1.25 1.5) [dry-kick]
            (ranged-rand 1.5 2) [dry-kick]}))

(bar)
(stop)
;; need bar map to change randomly every bar

; For every tick of the metronome, we loop through all our beats
; and find the apropriate one my taking the metronome tick mod 4.
; Then we play all the instruments for that beat.

(defn player
  [tick bar]
  (dorun
   (for [k (keys bar)]
      (let [beat (Math/floor k)
            offset (- k beat)]
           (if (== 0 (mod (- tick beat) 4))
               (let [instruments (bar k)]
                    (dorun
                      (for [instrument instruments]
                        (at (metro (+ offset tick)) (instrument))))))))))

;; define a run fn which will call our player fn for each beat and will schedule
;; itself to be called just before the next beat

(defn run-sequencer
  [m]
  (make-bar)
  (let [beat (m)]
    (player beat bar)
    (apply-at (m (inc beat))  #'run-sequencer [m])))

;; make beats! Edit bar whilst the beat is playing to make live changes.
(run-sequencer metro)


(stop)
;; Keyboard Stuff
(def kb (midi-in ))

(defn midi-player [event ts]
  (alien-pad (:note event) (/ (:vel event) 128.0)))

(midi-handle-events kb #'midi-player)

(event-debug-on)

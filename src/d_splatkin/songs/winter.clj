(ns d-splatkin.songs.winter
  (:use [overtone.live]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [overtone.gui.mixer]))


(def m (metronome (* 100 2)))

(definst beep [note 60]
  (let [freq (midicps note)
        src  (sin-osc freq)
        vib  (sin-osc (* freq 0.99))
        src  (/ (+ src vib) 2)
        env  (env-gen (perc 0.001 1 1 -4) 1 1 0 1 :action FREE)]
    (* src env)))

(definst bass-beep [note 60 vel 0.8]
  (let [freq (midicps note)
        src  (saw freq)
        vib  (sin-osc (* freq 0.99))
        src  (/ (+ src vib) 2)
        env  (env-gen (perc 0.001 0.5 1 -4) :action FREE)]
    (* vel src env)))
(stop)

(bass-beep)

(definst string [note 60 amp 1.0 dur 0.5 decay 30 coef 0.3 gate 1]
  (let [freq (midicps note)
        noize (* 0.8 (white-noise))
        dly   (/ 1.0 freq)
        plk   (pluck noize gate dly dly decay coef)
        dist  (distort plk)
        filt  (rlpf dist (* 12 freq) 0.6)
        clp   (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action 0) reverb)))

(def kb (midi-in "LPK25"))

(defn midi-player [event]
  (when (= (:status event) :note-on) (overpad (:note event) (/ (:velocity event) 128.0))))

(midi-handle-events kb #'midi-player)

(defn ice [beat]
  (let [next-phrase (+ beat 4)]
    (at (m beat) (beep 101))
    (at (m (+ beat 1)) (beep 104))
    (at (m (+ beat 2)) (beep 102))

    (apply-at (m next-phrase) #'ice [next-phrase])))


(defn bassline [beat]
  (let [next-phrase (+ beat 16)]
    (at (m beat) (bass-beep 41))
    (at (m (+ beat 1)) (bass-beep 41))
    (at (m (+ beat 2)) (bass-beep 41))
    (at (m (+ beat 3)) (bass-beep 41))
    (at (m (+ beat 3.25)) (bass-beep 41))

    (at (m (+ beat 6)) (bass-beep 41))
    (at (m (+ beat 6.5)) (bass-beep 41))
    (at (m (+ beat 8)) (bass-beep 42))
    (at (m (+ beat 9)) (bass-beep 42))
    (at (m (+ beat 10)) (bass-beep 42))
    (at (m (+ beat 11)) (bass-beep 42))
    (at (m (+ beat 12)) (bass-beep 44))

    (at (m (+ beat 14)) (bass-beep 44))


    (apply-at (m next-phrase) #'bassline [next-phrase])))

(defn drums [beat]
  (let [next-beat (+ beat 8)]
    (at (m beat) (closed-hat2))
    (at (m (+ beat 1))(closed-hat2))
    (at (m (+ beat 2))(closed-hat2))
    (at (m (+ beat 3)) (closed-hat2))
    (at (m (+ beat 4)) (closed-hat2))

    (at (m (+ beat 2)) (snare))
    (at (m (+ beat 6)) (snare))

    (at (m beat) (kick2))
    (at (m (+ beat 3)) (kick2))
    (at (m (+ beat 5)) (kick2))
    (at (m (+ beat 7)) (kick2))

    ;; add kick 4 with echo or blip of some sort
    (apply-at (m next-beat) #'drums [next-beat])))


(do
  (drums (m))
  (bassline (m))
  (ice (m)))
(stop)
(inst-pan! closed-hat2 0.50)
(inst-pan! snare -0.25)
(inst-volume! snare 1)
(inst-volume! closed-hat2 1)
(inst-volume! bass-beep 1)
(inst-volume! beep 0.1)
(inst-volume! overpad 0.2)
(def closed-hat-reverb (inst-fx! closed-hat2 fx-freeverb))
(def bass-fx (inst-fx! bass-beep fx-chorus))
(def bass-echo (inst-fx! bass-beep fx-echo))
(def ice-verb (inst-fx! beep fx-freeverb))
(def ice-echo (inst-fx! beep fx-echo))
(def overpad-chorus (inst-fx! overpad fx-chorus))

(clear-fx bass-beep)
(ctl closed-hat-reverb :wet-dry 0.40 :room-size 0.33 :dampening 0.2)
(ctl bass-echo :max-delay 0.5 :delay-time (/ 15000 100))
(ctl ice-echo :max-delay 1 :delay-time (/ 15000 100))
(ctl ice-verb :wet-dry 0.75 :room-size 0.5 :dampening 0.8)
(stop)
(volume 0.8)

;; 1/16 = 15,000 / # of bpm.
;; 3/16 = 45,000 / # of bpm <--- Edge’s favorite setting!
;;   (Testing with the example above: 45,000 / 100 bpm = 450ms which matches the above calculation).
;; 1/4 = 60,000 / # of bpm. (Edge rarely uses this)
;; These settings have only been used on one or two songs each:
;; 1/3 of a quarter note triplet (= 1/6) = 40,000 / # of bpm (‘Bullet’, ‘All I Want Is You’ solo) <- this delay time sounds ‘tense’ and unsettling.
;; 3/32 (‘Crumbs from your table’) = 22,500 / bpm (or half of 3/16).

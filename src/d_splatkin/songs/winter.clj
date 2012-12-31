(ns d-splatkin.songs.winter
  (:use [overtone.live]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [overtone.gui.mixer]))


(def m (metronome (* 100 2)))

(defsynth beep [note 60 vel 0.4]
  (let [freq (midicps note)
        src  (sin-osc freq)
        vib  (sin-osc (* freq 0.99))
        src  (/ (+ src vib) 2)
        env  (env-gen (perc 0.01 1 1 -4) 1 1 0 1 :action FREE)
        out1 (comb-l (* src env) 0.25 0.5 2)]
    (out 0 (* 0.5 [(* vel src env) (* vel out1)]))))

(defsynth bass-beep [note 60 vel 0.8]
  (let [freq (midicps note)
        src  (sin-osc freq)
        vib  (sin-osc (* freq 0.99))
        src  (/ (+ src vib) 2)
        env  (env-gen (perc 0.1 1 1 -4) 1 1 0 1 :action FREE)]
    (out 0 (* [0.5 0.5] (* vel src env)))))







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
    (at (m beat) (beep 89))
    (at (m (+ beat 1)) (beep 92))
    (at (m (+ beat 1.5)) (beep 90))

    (apply-at (m next-phrase) #'ice [next-phrase])))


(defn bassline [beat]
  (let [next-phrase (+ beat 16)]
    (at (m beat) (bass-beep 41))
    (at (m (+ beat 1)) (bass-beep 41))
    (at (m (+ beat 2)) (bass-beep 41))
    (at (m (+ beat 3)) (bass-beep 41))
    (at (m (+ beat 4)) (bass-beep 41))
    (at (m (+ beat 5)) (bass-beep 41))
    (at (m (+ beat 6)) (bass-beep 41))
    (at (m (+ beat 7)) (bass-beep 41))
    (at (m (+ beat 8)) (bass-beep 42))
    (at (m (+ beat 9)) (bass-beep 42))
    (at (m (+ beat 10)) (bass-beep 42))
    (at (m (+ beat 11)) (bass-beep 42))
    (at (m (+ beat 12)) (bass-beep 44))
    (at (m (+ beat 13)) (bass-beep 44))
    (at (m (+ beat 14)) (bass-beep 44))
    (at (m (+ beat 15)) (bass-beep 44))

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

(inst-pan! closed-hat2 0.50)
(inst-pan! snare -0.25)
(inst-volume! snare 1)
(inst-volume! closed-hat2 0.9)
(def closed-hat-reverb (inst-fx! closed-hat2 fx-freeverb))

(ctl closed-hat-reverb :wet-dry 0.40 :room-size 0.33 :dampening 0.2)
(stop)

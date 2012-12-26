(ns d-splatkin.songs.winter
  (:use [overtone.live]))

(definst beep [note 60]
  (let [freq (midicps note)
        src  (sin-osc freq)
        env  (env-gen (perc 0.01 1 1 -4) 1 1 0 1 :action FREE)]
    (* env src)))

(def kb (midi-in ))

(defn midi-player [event tsn]
  (beep (:note event) (/ (:vel event) 128.0)))

(midi-handle-events kb #'midi-player)

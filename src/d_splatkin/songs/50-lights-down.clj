(ns d-splatkin.songs.50-lights-down
  (:use d-splatkin.instruments
        overtone.music.pitch
        overtone.music.tuning))

(definst bass [note 28]
  (let [freq (midicps note)
        src (stk-pluck freq)]
    src))


(bass (note "e1"))
(stop)
(def e-string (vec (range 28 49)))
(def a-string (vec (range 33 53)))
(def d-string (vec (range 38 58)))
(def g-string (vec (range 42 63)))


(defn find-fret [string note-str]
  (let [note-num (note note-str)]
    (.indexOf string note-num)))
(note-pos "a1" e-string)

(find-note-name (d-string 2))
(find-note-name (d-string 1))
(find-note-name (a-string 3))
(find-note-name (a-string 0))
(find-note-name (d-string 5))
(find-note-name (d-string 4))

(scale :e1 :minor)

(def m (metronome 100))
(bass (d-string 2))

d-string 2 2 1
a-string 0 3 3 3 3 3 3
d-string 5 5 5 4 3
a-string 2 2 2 2 2 2 2 2

()
(player m)

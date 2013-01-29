(ns d-splatkin.instruments
  (:use [overtone.live]))

(definst bleep [note 60 amp 0.2]
  (let [freq (midicps note)
        f-env (line:kr 0 1 0.25 :action FREE)]
    (* amp (square (* freq f-env)))))


(definst drops
  [freq   10000
   amp    {:default 0.6 :min 0.001 :max 1 :step 0.01}
   attack {:default 0.001 :min 0.001 :max 1.0 :step 0.0001}
   decay  {:default 0.01 :min 0.001 :max 1 :step 0.001}]
  (let [env (env-gen (perc attack decay) :action FREE)
        snd (sin-osc freq)
        snd (* amp env snd)]
    snd))

;;http://computermusicresource.com/Simple.bell.tutorial.html
(def dull-partials
  [
   0.56
   0.92
   1.19
   1.71
   2
   2.74
   3
   3.76
   4.07])

;; http://www.soundonsound.com/sos/Aug02/articles/synthsecrets0802.asp
;; (fig 8)
(def partials
  [
   0.5
   1
   3
   4.2
   5.4
   6.8])

;; we make a bell by combining a set of sine waves at the given
;; proportions of the frequency. Technically not really partials
;; as for the 'pretty bell' I stuck mainly with harmonics.
;; Each partial is mixed down proportional to its number - so 1 is
;; louder than 6. Higher partials are also supposed to attenuate
;; quicker but setting the release didn't appear to do much.

(defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   dur  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (env-gen (perc 0.01 (* dur proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))


(definst dull-bell [note 60 dur 1.0 vol 1.0]
  (let [freq (midicps note)
        snd (* vol (bell-partials freq dur dull-partials))]
    (detect-silence snd :action FREE)
    snd))

(definst pretty-bell [note 60 dur 1.0 vol 1.0]
  (let [freq (midicps note)
        snd (* vol (bell-partials freq dur partials))]
    (detect-silence snd :action FREE)
    snd))

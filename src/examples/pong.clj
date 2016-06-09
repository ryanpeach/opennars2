(ns examples.pong
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [gui.hnav :as hnav]
            [narjure.core :as nar]
            [narjure.sensorimotor :refer :all]))

(def py (atom 50))

(defn setup-pong []
  (nars-input-narsese (str "<(*,{SELF}) --> op_up>!" ))
  (nars-input-narsese (str "<(*,{SELF}) --> op_down>!" ))
  (nars-input-narsese "<{SELF} --> [good]>!")
  (q/frame-rate 10)
  (nars-register-operation 'op_up (fn [args]
                                    (reset! py (+ @py 100))))
  (nars-register-operation 'op_down (fn [args]
                                      (reset! py (- @py 100))))
  (merge hnav/states {:ball-px 80
                      :ball-py 280
                      :direction-x 1
                      :direction-y 1
                      :barheight 100
                      :iteration 0}))

(def fieldmax 760)
(def fieldmin 20)
(def allow-continuous-feedback true)

(defn update-pong
  [state]

  (when (= (mod (:iteration state) 100) 0)
    (nars-input-narsese "<{SELF} --> [good]>!")
    (nars-input-narsese (str "<(*,{SELF}) --> op_up>!" ))
    (nars-input-narsese (str "<(*,{SELF}) --> op_down>!" )))

  (when (= (mod (:iteration state) 10) 0)
    #_(nars-input-narsese (str "<{" (int (* 100 (quot (:ball-py state) 100))) "} --> ballpos>. :|:" ))
    #_(nars-input-narsese (str "<{" (int (* 100 (quot @py 100))) "} --> barpos>. :|:" ))
      (if (and (>= (:ball-py state) @py)
               (<= (:ball-py state) (+ @py (:barheight state))))
        (do (nars-input-narsese (str "<ballpos --> [equal]>. :|:"))
            (when allow-continuous-feedback
              (println "good NARS")
              (nars-input-narsese "<{SELF} --> [good]>. :|: %1.0;0.9%")))
        (if (< (:ball-py state) @py)
          (do
            (nars-input-narsese (str "<ballpos --> [below]>. :|:"))
            (when allow-continuous-feedback
              (println "bad NARS")
              (nars-input-narsese "<{SELF} --> [good]>. :|: %0.0;0.9%")))
          (do
            (nars-input-narsese (str "<ballpos --> [above]>. :|:"))
            (when allow-continuous-feedback
              (println "bad NARS")
              (nars-input-narsese "<{SELF} --> [good]>. :|: %0.0;0.9%"))))
        )
    )

  (let [kset-x (+ 0.6 (/ (Math/random) 2.0))
        kset-y (+ 0.6 (/ (Math/random) 2.0))
        state2 (assoc state
                 :ball-px (+ (:ball-px state) (* (:direction-x state) 1 3))
                 :ball-py (+ (:ball-py state) (* (:direction-y state) 1 3)))

        state3 (if (>= (:ball-px state2)                     ;collided on right wall
                      fieldmax)
                 (assoc state2 :direction-x (- kset-x))
                 state2)

        state4 (if (<= (:ball-px state3)                     ;collided on left wall!!
                      fieldmin)
                 (do
                   (nars-input-narsese "<{SELF} --> [good]>. :|: %0.0;0.9%")
                   (nars-input-narsese "<{SELF} --> [good]>!")
                   (println "bad NARS")
                   (assoc state3 :direction-x kset-x))
                 state3)

        state5 (if (>= (:ball-py state4)                     ;collided on upper wall
                       fieldmax)
                 (assoc state4 :direction-y (- kset-y))
                 state4)

        state6 (if (<= (:ball-py state5)                     ;collided on down wall
                       fieldmin)
                 (assoc state5 :direction-y kset-y)
                 state5)

        state7 (if (and (<= (:ball-px state6) 40)            ;got it
                        (>= (:ball-py state6) @py)
                        (<= (:ball-py state6) (+ @py (:barheight state6))))
                 (do
                   (nars-input-narsese "<{SELF} --> [good]>. :|: %1.0;0.9%")
                   (nars-input-narsese "<{SELF} --> [good]>!")
                   (println "good NARS")
                   (assoc state6 :direction-x kset-x))
                 state6)]

    (when (> @py fieldmax)
      (reset! py fieldmax))
    (when (< @py fieldmin)
      (reset! py fieldmin))

    (assoc state7
      :ball-px (max fieldmin (min (:ball-px state7) fieldmax))
      :ball-py (max fieldmin (min (:ball-py state7) fieldmax))
      :iteration (inc (:iteration state7)))))


(defn draw-pong [state]
  (q/background 255)
  (q/reset-matrix)
  (hnav/transform state)
  (q/rect fieldmin fieldmin fieldmax fieldmax)
  (q/rect 10 @py 10 (:barheight state))
  (q/rect (:ball-px state) (:ball-py state) 10 10))

(q/defsketch pong
             :size [(hnav/width) (hnav/height)]
             :setup setup-pong
             :draw draw-pong
             :update update-pong
             :mouse-pressed (partial hnav/mouse-pressed [] {})
             :mouse-dragged hnav/mouse-dragged
             :mouse-wheel hnav/mouse-wheel
             :middleware [m/fun-mode]
             :features [ :resizable ]
             :title "OpenNARS 2.0.0: Pong")
(ns gui.hud
  (:require [seesaw.core :refer :all]
            [gui.globals :refer :all]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
            [clojure.string :as str]
            [narjure.core :refer [start-timers shutdown run stop-timers]]
            [gui.gui-utils :refer :all]
            [narjure.defaults :refer :all]))

(def backcolor [230 230 230 100])
(def hud-width 50)
(def hud-height 25)

(def nodes [{:name :pop-up :px 700 :py 0 :onclick (fn [state]
                                                       (cast! (whereis :sentence-parser) [:narsese-string-msg (str (input "Add Narsese" :to-string :name) "\n")]))
             :backcolor backcolor}
            {:name :paste :px 750 :py 0 :onclick (fn [state]
                                                      (input-str (slurp-clipboard)))
             :backcolor backcolor}

            {:name :resume :px 0 :py 0 :onclick (fn [state]
                                                       (start-timers))
             :displaysize 10
             :backcolor backcolor}
            {:name :pause :px 50 :py 0 :custom-w 40 :onclick (fn [state]
                                                     (stop-timers))
             :backcolor backcolor}
            {:name :step :custom-w 35 :px 90 :py 0 :onclick (fn [state]
                                                               (cast! (whereis :concept-selector) [:inference-tick-msg])
                                                               (cast! (whereis :task-creator) [:system-time-tick-msg])
                                                               (cast! (whereis :derived-load-reducer) [:system-time-tick-msg])
                                                               (cast! (whereis :forgettor) [:system-time-tick-msg]))
             :backcolor backcolor}
            {:name :off :custom-w 25 :px 125 :py 0 :onclick (fn [state]
                                                    (shutdown))
             :backcolor backcolor}
            {:name :start :px 150 :py 0 :onclick (fn [state]
                                                      (run))
             :backcolor backcolor}

            {:name :color :px 200 :py 0 :onclick (fn [state]
                                                       (reset! invert-colors (not @invert-colors)))
             :backcolor backcolor}
            {:name :speed :px 250 :py 0 :onclick (fn [state]
                                                   (reset! speed
                                                           (if (= @speed "slow")
                                                             (do
                                                               (reset! system-tick-interval system-tick-interval-fast)
                                                               (reset! inference-tick-interval inference-tick-interval-fast)
                                                               "fast")
                                                             (do
                                                               (reset! system-tick-interval system-tick-interval-slow)
                                                               (reset! inference-tick-interval inference-tick-interval-slow)
                                                               "slow")))
                                                   (stop-timers)
                                                   (start-timers))
             :displaysize 10
             :backcolor backcolor}
            {:name :+prioTh. :px 300 :py 0 :onclick (fn [state]
                                                      (reset! prio-threshold (+ @prio-threshold 0.1)))
             :displaysize 10
             :backcolor backcolor}
            {:name :-prioTh. :px 350 :py 0 :onclick (fn [state]
                                                            (reset! prio-threshold (- @prio-threshold 0.1)))
             :backcolor backcolor}
            {:name :details :px 400 :py 0 :onclick (fn [state]
                                                            (reset! link-labels (not @link-labels)))
             :backcolor backcolor}

            {:name :send :px 650 :py 0 :onclick (fn [state]
                                                     (input-str (deref inputstr))
                                                     (swap! inputstr (fn [st] ""))) :backcolor backcolor}

            {:name :input :px 450 :custom-w 200 :py 0 :backcolor backcolor :displaysize 10}
            ])

(def hud [nodes [] hud-width hud-height])
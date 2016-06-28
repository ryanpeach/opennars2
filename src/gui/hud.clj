(ns gui.hud
  (:require [seesaw.core :refer :all]
            [gui.globals :refer :all]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
            [clojure.string :as str]
            [narjure.core :refer [start-timers shutdown run stop-timers]]
            [gui.gui-utils :refer :all]))

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
             :backcolor backcolor}
            {:name :pause :px 50 :py 0 :onclick (fn [state]
                                                     (stop-timers))
             :backcolor backcolor}
            {:name :off :px 100 :py 0 :onclick (fn [state]
                                                    (shutdown))
             :backcolor backcolor}
            {:name :start :px 150 :py 0 :onclick (fn [state]
                                                      (run))
             :backcolor backcolor}

            {:name :color :px 225 :py 0 :onclick (fn [state]
                                                       (reset! invert-colors (not @invert-colors)))
             :backcolor backcolor}
            {:name :+prioTh. :px 275 :py 0 :onclick (fn [state]
                                                            (reset! prio-threshold (+ @prio-threshold 0.1)))
             :displaysize 10
             :backcolor backcolor}
            {:name :-prioTh. :px 325 :py 0 :onclick (fn [state]
                                                            (reset! prio-threshold (- @prio-threshold 0.1)))
             :backcolor backcolor}
            {:name :details :px 375 :py 0 :onclick (fn [state]
                                                            (reset! link-labels (not @link-labels)))
             :backcolor backcolor}

            {:name :send :px 650 :py 0 :onclick (fn [state]
                                                     (input-str (deref inputstr))
                                                     (swap! inputstr (fn [st] ""))) :backcolor backcolor}

            {:name :input :px 450 :custom-w 200 :py 0 :backcolor backcolor :displaysize 10}
            ])

(def hud [nodes [] hud-width hud-height])
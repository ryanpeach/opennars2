(ns gui.gui
  (:require [seesaw.core :refer :all]
            [gui.globals :refer :all]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
            [narjure.core :refer [start-timers shutdown run stop-timers]]))

(def backcolor [200 200 200])
(def gui-width 50)
(def gui-height 25)
(def inputstr (atom ""))                                    ;py -525 with input load reducer
(def nodes [{:name :send :px 500 :py -325 :onclick (fn [state]
                                                     (println (str "input narsese " (deref inputstr)))
                                                     (cast! (whereis :sentence-parser) [:narsese-string-msg (deref inputstr)])
                                                     (swap! inputstr (fn [st] ""))) :backcolor backcolor}
            {:name :clear :px 400 :py -325 :onclick (fn [state]
                                                     (swap! inputstr (fn [st] ""))) :backcolor backcolor}
            {:name :putin :px 450 :py -325 :onclick (fn [state]
                                                      (swap! inputstr (fn [st]
                                                                            (str (input "Add Narsese" :to-string :name) "\n"))))
             :backcolor backcolor}

            {:name :resume :px 450 :py -525 :onclick (fn [state]
                                                      (start-timers))
             :backcolor backcolor}
            {:name :stop :px 500 :py -525 :onclick (fn [state]
                                                       (stop-timers))
             :backcolor backcolor}
            {:name :shutdown :px 650 :py -525 :onclick (fn [state]
                                                       (shutdown))
             :backcolor backcolor}
            {:name :start :px 700 :py -525 :onclick (fn [state]
                                                       (run))
             :backcolor backcolor}

            {:name :inv-col :px 1000 :py -525 :onclick (fn [state]
                                                      (reset! invert-colors (not @invert-colors)))
             :backcolor backcolor}])

(def graph-gui [nodes [] gui-width gui-height])
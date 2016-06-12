(ns gui.gui
  (:require [seesaw.core :refer :all]
            [gui.globals :refer :all]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
            [clojure.string :as str]
            [narjure.core :refer [start-timers shutdown run stop-timers]]))

(def backcolor [230 230 230])
(def gui-width 50)
(def gui-height 25)

(defn input-str [st]
  (println (str "input narsese " st))
  (doseq [x (str/split st #"\n")]
    (cast! (whereis :sentence-parser) [:narsese-string-msg x])))

(def inputstr (atom ""))                                    ;py -525 with input load reducer
(def nodes [{:name :send :px 500 :py -325 :onclick (fn [state]
                                                     (input-str (deref inputstr))
                                                     (swap! inputstr (fn [st] ""))) :backcolor backcolor}
            {:name :paste :px 350 :py -325 :onclick (fn [state]
                                                     (input-str (slurp-clipboard)))
             :backcolor backcolor}
            {:name :clear :px 400 :py -325 :onclick (fn [state]
                                                     (swap! inputstr (fn [st] ""))) :backcolor backcolor}
            {:name :pop-up :px 450 :py -325 :onclick (fn [state]
                                                      (cast! (whereis :sentence-parser) [:narsese-string-msg (str (input "Add Narsese" :to-string :name) "\n")]))
             :backcolor backcolor}

            {:name :resume :px 450 :py -525 :onclick (fn [state]
                                                      (start-timers))
             :backcolor backcolor}
            {:name :stop :px 500 :py -525 :onclick (fn [state]
                                                       (stop-timers))
             :backcolor backcolor}
            {:name :off :px 650 :py -525 :onclick (fn [state]
                                                       (shutdown))
             :backcolor backcolor}
            {:name :start :px 700 :py -525 :onclick (fn [state]
                                                       (run))
             :backcolor backcolor}

            {:name :color :px 1000 :py -525 :onclick (fn [state]
                                                      (reset! invert-colors (not @invert-colors)))
             :backcolor backcolor}
            {:name :+prioThres :px 1100 :py -525 :onclick (fn [state]
                                                       (reset! prio-threshold (+ @prio-threshold 0.1)))
             :backcolor backcolor}
            {:name :-prioThres :px 1200 :py -525 :onclick (fn [state]
                                                            (reset! prio-threshold (- @prio-threshold 0.1)))
             :backcolor backcolor}
            {:name :linkLabels :px 1300 :py -525 :onclick (fn [state]
                                                       (reset! link-labels (not @link-labels)))
             :backcolor backcolor}])

(def graph-gui [nodes [] gui-width gui-height])
(ns narjure.sensorimotor
  (:require [narjure.perception-action.operator-executor :refer [registered-operator-functions]]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
            [narjure.global-atoms :refer [answer-handlers]]))

(defn nars-register-operation [k f]
  (reset! registered-operator-functions (assoc @registered-operator-functions k f)))

(defn nars-register-answer-handler [f]
  (reset! answer-handlers (concat @answer-handlers [f])))

(defn nars-input-narsese [str]
  (cast! (whereis :sentence-parser) [:narsese-string-msg str]))
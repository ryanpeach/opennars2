(ns narjure.sensorimotor
  (:require [narjure.perception-action.operator-executor :refer [registered-operator-functions]]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]))

(defn nars-register-operation [k f]
  (reset! registered-operator-functions (assoc @registered-operator-functions k f)))

(defn nars-input-narsese [str]
  (println str)
  (cast! (whereis :sentence-parser) [:narsese-string-msg str]))
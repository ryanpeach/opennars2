(ns narjure.memory-management.concept-utils
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.global-atoms :refer :all]
    [narjure.bag :as b]
    [clojure.core.unify :refer [unifier]]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.defaults :refer :all]
    [nal.term_utils :refer [syntactic-complexity]]
    [narjure.memory-management.local-inference.local-inference-utils :refer [get-task-id get-tasks]]
    [narjure.memory-management.local-inference.belief-processor :refer [process-belief]]
    [narjure.memory-management.local-inference.goal-processor :refer [process-goal]]
    [narjure.memory-management.local-inference.quest-processor :refer [process-quest]]
    [narjure.memory-management.local-inference.question-processor :refer [process-question]]
    [nal.deriver.truth :refer [w2c t-or t-and confidence frequency expectation revision]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))

(defn concept-quality []
  (:quality ((:elements-map @c-bag) (:id @state))))

(defn concept-priority [term]
  (:priority ((:elements-map @c-bag) term)))

(defn get-ref-from-term [term]
  (:ref ((:elements-map @c-bag) term)))

(defn forget-task [el last-forgotten]
  (let [task (:task el)
        budget (:budget task)
        lambda (/ (- 1.0 (second budget)) decay-rate)
        fr (Math/exp (* -1.0 (* lambda (- @nars-time last-forgotten))))
        new-priority (max (round2 4 (* (:priority el) fr))
                          (/ (concept-quality) (+ 1.0 (b/count-elements (:tasks @state)))) ;dont fall below 1/N*concept_quality
                          (nth budget 2)) ;quality of task
        new-budget [new-priority (second budget) (nth budget 2)]]
    (let [updated-task (assoc task :budget new-budget)]
      (assoc el :priority new-priority
                :task updated-task))))

(defn forget-tasks []
  (let [tasks (:elements-map (:tasks @state))
        last-forgotten (:last-forgotten @state)]
    (set-state! (assoc @state :tasks (b/default-bag max-tasks)))
    (doseq [[id el] tasks]                       ;{ id {:staement :type :occurrence}
      (let [el' (forget-task el last-forgotten)]
        ;(println (str "forgetting: " (get-in el' [:task :statement])))
        (set-state! (assoc @state :tasks (b/add-element (:tasks @state) el')))))
    (set-state! (assoc @state :last-forgotten @nars-time))))
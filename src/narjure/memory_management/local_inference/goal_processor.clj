(ns narjure.memory-management.local-inference.goal-processor
  (:require
    [co.paralleluniverse.pulsar.actors
     :refer [! spawn gen-server register! cast! Server self
             shutdown! unregister! set-state! state whereis]]
    [narjure.actor.utils :refer [defactor]]
    [taoensso.timbre :refer [debug info]]
    [narjure.bag :as b]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.global-atoms :refer :all]
    [narjure.memory-management.local-inference.local-inference-utils :refer :all]
    [nal.deriver.truth :refer [t-or frequency confidence expectation]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))


(defn satisfaction-based-budget-change [state goal-task beliefs]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence goal-task) a @nars-time)])
             (filter #(= (:statement %) (:statement goal-task)) beliefs))]
    (when (not-empty projected-list)
      (doseq [[belief belief-task-projected-to-goal] projected-list]
        (when (better-solution belief goal-task)
          ;update budget and solution
          (let [new-goal (reduced-goal-budget-by-belief goal-task belief-task-projected-to-goal)
                new-goal-with-solution (assoc new-goal :solution belief)]
            (update-task-in-tasks state new-goal-with-solution goal-task))
          (let [new-belief (increased-belief-budget-by-goal belief-task-projected-to-goal goal-task)]
            (update-task-in-tasks state new-belief belief)))))))


(defn operation? [task]
  (let [st (:statement task)]
    (if (and (= (first st) '-->)
                    (coll? (second st))
                    (= (first (second st)) '*))
      (let [op (nth st 2)]
        (and (clojure.string/starts-with? (name op) "op_")))
      false)))

(def decision-threshold 0.5)

(defn execute? [task]
  (> (expectation (:truth task)) decision-threshold))

(defn answer-based-budget-change [state goal-task quests]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence a) goal-task @nars-time)])
             (filter #(= (:statement %) (:statement goal-task)) quests))]
    (when (not-empty projected-list)
      (doseq [[quest goal-task-projected-to-quest] projected-list]
        (when (better-solution goal-task quest)
          ;update budget and solution
          (let [new-quest (reduced-quest-budget-by-goal quest goal-task-projected-to-quest)
                new-quest-with-solution (assoc new-quest :solution goal-task)]
            (update-task-in-tasks state new-quest-with-solution quest))
          (let [new-goal (increased-goal-budget-by-quest goal-task-projected-to-quest quest)]
            (update-task-in-tasks state new-goal goal-task)))))))

(defn process-goal [state task]
  ;group-by :task-type tasks
  (let [goals (filter #(= (:task-type %) :goal) (:tasks @state))
        beliefs (filter #(= (:task-type %) :belief) (:tasks @state))
        quests (filter #(= (:task-type %) :quest ) (:tasks @state))]

    ;filter beliefs matching concept content
    ;(project to task time
    (let [projected-goals (map #(project-eternalize-to (:occurrence task) % @nars-time) (filter #(= (:statement %) (:statement task)) goals))]
      ;revise task with revisable goals
      (doseq [revisable (filter #(revisable? task %) projected-goals)]
        ;revise goals and add to tasks
        (process-goal state (revise task revisable))))

    ;add task to bag
    (add-to-tasks state task)
    ; check to see if revised or task is answer to quest and increase budget accordingly
    ;check whether it is fullfilled by belief and decrease budget accordingly
    (satisfaction-based-budget-change state task beliefs)
    (answer-based-budget-change state task quests)

    ;best operation project goal to current time
    ; if above decision threshold then execute
    (let [projected-goals (map #(project-eternalize-to @nars-time % @nars-time)
                               (filter #(= (:statement %) (:statement task))
                                       (filter #(= (:task-type %) :goal) ;re-getting the goals because we also want our just added goal
                                               (apply vector (for [x (:priority-index (:tasks @state))] (:id x))))))]
     (if (not-empty projected-goals)
       (let [goal (reduce #(max (confidence %)) projected-goals)]
         (when (and (operation? goal)
                    (= (:statement goal) (:id @state)))   ;execution really only in concept which is responsible for this goal!
           (when (execute? goal)
             (cast! (whereis :operator-executor) [:operator-execution-msg goal]))))))

    ))
